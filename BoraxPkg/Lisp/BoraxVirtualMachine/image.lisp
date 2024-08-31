(uiop:define-package :borax-virtual-machine/image
  (:nicknames :borax-vm/image)
  (:mix :closer-mop :uiop/common-lisp :borax-virtual-machine/common-lisp)
  (:use :cl-locatives)
  (:export ;; memory-model
           #:memory-model #:+32-bit+ #:+64-bit+
           #:page-bytes #:word-bits #:word-bytes #:word-type
           #:cons-first-word #:object-first-word
           ;; immediate
           #:immediate #:immediate-value #:+unbound+
           ;; image
           #:image #:with-image #:*image* #:objects #:root
           #:image-class #:image-class-locative
           #:reify #:reify-place #:reify-image
           ;; object
           #:object #:index #:sub-objects
           ;; borax-vm/cl:class
           #:+classes+ #:default-direct-superclass
           ;; record-object
           #:record-object #:record-class
           #:record-slots #:do-record-slots
           ;; record-vector
           #:record-vector #:vector-data))

(in-package :borax-virtual-machine/image)

(defstruct (memory-model (:constructor %make-memory-model) :conc-name)
  (page-bytes           0 :type fixnum         :read-only t)
  (word-bits            0 :type (member 32 64) :read-only t)
  (word-bytes           0 :type (member 4 8)   :read-only t)
  (word-type            t                      :read-only t)
  (borax-vm/cl:most-positive-fixnum
                        0 :type integer        :read-only t)
  (borax-vm/cl:most-negative-fixnum
                        0 :type integer        :read-only t)
  (cons-first-word      0 :type fixnum         :read-only t)
  (object-first-word    0 :type fixnum         :read-only t))

(defun make-memory-model (word-bits)
  (let* ((page-bytes 4096)
         (word-bytes (floor word-bits 8))
         (cons-bytes (* 2 word-bytes))
         (cons-per-page (floor page-bytes cons-bytes))
         (cons-bitmap-words (floor cons-per-page word-bits))
         (cons-header-words (+ 2 cons-bitmap-words)))
    (%make-memory-model
     :page-bytes page-bytes
     :word-bits word-bits
     :word-bytes word-bytes
     :word-type `(unsigned-byte ,word-bits)
     :most-positive-fixnum (- (ash 1 (- word-bits 2)) 1)
     :most-negative-fixnum (- (ash 1 (- word-bits 2)))
     :cons-first-word cons-header-words
     :object-first-word 2)))

(defvar +32-bit+ (make-memory-model 32))
(defvar +64-bit+ (make-memory-model 64))

(defconstant +initial-space-size+ 100)

(defun make-space (&optional (initial-size +initial-space-size+))
  (make-array initial-size :fill-pointer 0 :adjustable t))

(defclass immediate ()
  ((value :type integer
          :reader immediate-value
          :initarg :value)))

(defvar +unbound+ (make-instance 'immediate :value #x5))

(define-symbol-macro +classes+
  (slot-value (class-prototype (find-class 'borax-vm/cl:class)) 'classes))

(defclass image ()
  ((memory-model :type memory-model
                 :reader memory-model
                 :initarg :memory-model)
   (objects :type (vector *)
            :reader objects
            :initform (make-space))
   (classes :type (vector *)
            :initarg :classes)
   (root :accessor root
         :initform +unbound+)))

(defvar *image* nil)

(defun make-image (memory-model)
  (make-instance 'image :memory-model memory-model
                 :classes (make-array (length +classes+)
                                      :initial-element nil
                                      :adjustable t)))

(defmacro with-image (memory-model &body body)
  `(progn
     (assert (null *image*))
     (let ((*image* (make-image ,memory-model)))
       ,@body)))

(defclass object ()
  ((color :type (member white grey black)
          :accessor color
          :initform 'white)
   (index :type (integer 0 *)
          :accessor index)))

(defmethod initialize-instance :after ((instance object) &key)
  (assert (not (null *image*)))
  (setf (index instance) (vector-push-extend instance (objects *image*))))

(defgeneric sub-objects (object))

(defgeneric reify (object)
  (:method ((object immediate)) object)
  (:method ((object object)) object)
  ;; TODO: Arbitrary-precision integers
  (:method ((object integer)) object)
  (:method ((object character)) object))

(define-modify-macro reify-place ()
  reify)

(defun reify-image ()
  (with-slots (objects root) *image*
    ;; Mark
    (let ((grey-list nil))
      (flet ((mark-grey (object)
               (when (typep object 'object)
                 (when (eq (color object) 'white)
                   (setf (color object) 'grey)
                   (push object grey-list))))
             (mark-black (object)
               (setf (color object) 'black)))
        ;; Mark roots grey
        (reify-place root)
        (mark-grey root)
        ;; Scan sub-objects then mark black
        (do ((object (pop grey-list) (pop grey-list)))
            ((null object))
          (dolist (loc (sub-objects object))
            (reify-place (dereference loc))
            (mark-grey (dereference loc)))
          (mark-black object))))
    ;; Compact
    (do* ((source 0 (1+ source))
          (destination 0))
         ((= source (length objects))
          (setf (fill-pointer objects) destination))
      (let ((object (aref objects source)))
        (ecase (color object)
          (white)
          (black
           (setf (aref objects destination) object)
           (setf (index object) destination)
           (setf (color object) 'white)
           (incf destination)))))))

(defclass borax-vm/cl:class (standard-class)
  ((classes :type (vector *)
            :allocation :class
            :initform (make-space 10))
   (index :type fixnum
          :accessor index)))

(defmethod initialize-instance :after ((class borax-vm/cl:class) &key &allow-other-keys)
  (setf (index class) (vector-push-extend class +classes+)))

(defgeneric default-direct-superclass (class)
  (:method ((class borax-vm/cl:class)) (find-class 'object)))

(defun default-initialize-instance (call-next-method class initargs)
  (destructuring-bind (&rest initargs &key direct-superclasses &allow-other-keys)
      initargs
    (apply call-next-method class
           :direct-superclasses (or direct-superclasses
                                    (list (default-direct-superclass class)))
           initargs)))

(defmethod initialize-instance :around ((class borax-vm/cl:class) &rest initargs)
  (default-initialize-instance #'call-next-method class initargs))

(defmethod reinitialize-instance :around ((class borax-vm/cl:class) &rest initargs)
  (default-initialize-instance #'call-next-method class initargs))

(defmethod validate-superclass ((class borax-vm/cl:class) superclass)
  (subclassp superclass (default-direct-superclass class)))

(defun ensure-image-class (class)
  (with-slots (index) class
    (with-slots (classes) *image*
      (unless (> (length classes) index)
        (adjust-array classes (1+ index) :initial-element nil))
      (when (null (aref classes index))
        (setf (aref classes index)
              (make-instance 'borax-vm/cl:standard-class
                             :name (symbol-name (class-name class)))))
      (values classes index))))

(defun image-class (class)
  (multiple-value-bind (classes index)
      (ensure-image-class class)
    (aref classes index)))

(defun image-class-locative (class)
  (multiple-value-bind (classes index)
      (ensure-image-class class)
    (locative-for (aref classes index))))

(defmethod sub-objects :around ((object object))
  ;; TODO: This hack is needed for mock-record, which should be removed.
  (if (typep (class-of object) 'borax-vm/cl:class)
      (cons (image-class-locative (class-of object))
            (call-next-method))
      (call-next-method)))

(defmethod reify ((object borax-vm/cl:class))
  (image-class object))

(defclass borax-vm/cl:cons ()
  ((borax-vm/cl:car :accessor borax-vm/cl:car
                    :initarg :car)
   (borax-vm/cl:cdr :accessor borax-vm/cl:cdr
                    :initarg :cdr))
  (:metaclass borax-vm/cl:class))

(defun borax-vm/cl:cons (car cdr)
  (make-instance 'borax-vm/cl:cons :car car :cdr cdr))

(defmethod sub-objects ((object borax-vm/cl:cons))
  (list (locative-for (borax-vm/cl:car object))
        (locative-for (borax-vm/cl:cdr object))))

(defmethod reify ((object cons))
  (borax-vm/cl:cons (car object) (cdr object)))

(defmacro borax-vm/cl:push (obj place &environment env)
  (multiple-value-bind (vars vals store-vars set get)
      (get-setf-expansion place env)
    (destructuring-bind (store-var) store-vars
      `(let* (,@(mapcar #'list vars vals)
              (,store-var (borax-vm/cl:cons ,obj ,get)))
         ,set))))

(defclass record-object (object) ())

(defclass record-class (borax-vm/cl:class)
  ((record-slots :type list
                 :accessor record-slots)))

(defmethod default-direct-superclass ((class record-class))
  (find-class 'record-object))

(defclass record-direct-slot-definition (standard-direct-slot-definition) ())

(defmethod direct-slot-definition-class ((class record-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'record-direct-slot-definition))

(defclass record-effective-slot-definition (standard-effective-slot-definition)
  ((record-slot-p :type boolean
                  :accessor record-slot-p)
   (record-slot-location :type fixnum
                         :accessor record-slot-location)))

(defmethod effective-slot-definition-class ((class record-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'record-effective-slot-definition))

(defmethod compute-effective-slot-definition :around
    ((class record-class) name direct-slot-definitions)
  (let ((effective-slot (call-next-method)))
    (setf (record-slot-p effective-slot)
          (typep (first direct-slot-definitions)
                 'record-direct-slot-definition))
    effective-slot))

(defun compute-slot-order (class)
  (let ((names ()))
    (dolist (c (reverse (class-precedence-list class)))
      (dolist (slot (class-direct-slots c))
        (pushnew (slot-definition-name slot) names)))
    (nreverse names)))

(defmethod compute-slots :around ((class record-class))
  (let ((slot-order (compute-slot-order class))
        (effective-slots (call-next-method)))
    (loop with i = 0
          for slot-name in slot-order
          for slot = (find slot-name effective-slots :key #'slot-definition-name)
          when (record-slot-p slot)
            do (setf (record-slot-location slot)
                     (prog1 i (incf i)))
            and collect slot into record-slots
          finally (setf (record-slots class) record-slots))
    effective-slots))

(defmacro do-record-slots (lambda-list object &body body)
  (destructuring-bind (place-name &optional (slot-name (gensym))) lambda-list
    (let ((object-name (gensym))
          (class-name (gensym)))
      `(let* ((,object-name ,object)
              (,class-name (class-of ,object-name)))
         (dolist (,slot-name (record-slots ,class-name))
           (symbol-macrolet ((,place-name (slot-value-using-class ,class-name
                                                                  ,object-name
                                                                  ,slot-name)))
             ,@body))))))

(defmethod sub-objects ((object record-object))
  (let ((result nil))
    (do-record-slots (value) object
      (push (locative-for value) result))
    (nreverse result)))

(defclass borax-vm/cl:standard-class ()
  ((name :initarg :name))
  (:metaclass record-class))

(defclass record-vector ()
  ((vector-data :type vector
                :accessor vector-data
                :initarg :data))
  (:metaclass borax-vm/cl:class))

(defmethod sub-objects ((object record-vector))
  (loop with vector-data = (vector-data object)
        for i from 0 below (length vector-data)
        collect (locative-for (aref vector-data i))))

(defmethod reify ((object vector))
  ;; Copy the vector so we can reify its contents.
  ;;
  ;; TODO: COPY-SEQ may be a more appropriate implementation for specialized
  ;; vector types.
  (make-instance 'record-vector :data (make-array (length object)
                                                  :initial-contents object)))

(defclass borax-vm/cl:string (record-vector)
  ()
  (:metaclass borax-vm/cl:class))

(defmethod sub-objects ((object borax-vm/cl:string))
  nil)

(defmethod reify ((object string))
  (make-instance 'borax-vm/cl:string :data object))
