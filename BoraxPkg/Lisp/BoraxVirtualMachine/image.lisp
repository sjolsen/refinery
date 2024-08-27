(uiop:define-package :borax-virtual-machine/image
  (:nicknames :borax-vm/image)
  (:use :uiop/common-lisp)
  (:shadow #:most-positive-fixnum #:most-negative-fixnum
           #:cons #:car #:cdr #:push)
  (:export ;; memory-model
           #:memory-model #:+32-bit+ #:+64-bit+
           #:page-bytes #:word-bits #:word-bytes #:word-type
           #:most-positive-fixnum #:most-negative-fixnum
           #:cons-first-word #:object-first-word
           ;; image
           #:image #:with-image #:*image* #:objects #:collect
           ;; object
           #:object #:index #:sub-objects
           #:cons #:car #:cdr #:push))

(in-package :borax-virtual-machine/image)

(defstruct (memory-model (:constructor %make-memory-model) :conc-name)
  (page-bytes           0 :type fixnum         :read-only t)
  (word-bits            0 :type (member 32 64) :read-only t)
  (word-bytes           0 :type (member 4 8)   :read-only t)
  (word-type            t                      :read-only t)
  (most-positive-fixnum 0 :type integer        :read-only t)
  (most-negative-fixnum 0 :type integer        :read-only t)
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

(defun make-space (&optional initial-size)
  (make-array (or initial-size +initial-space-size+)
              :fill-pointer 0 :adjustable t))

(defclass image ()
  ((memory-model :type memory-model
                 :reader memory-model
                 :initarg :memory-model)
   (objects :type (vector *)
            :reader objects
            :initform (make-space))))

(defvar *image* nil)

(defmacro with-image (memory-model &body body)
  `(progn
     (assert (null *image*))
     (let ((*image* (make-instance 'image :memory-model ,memory-model)))
       ,@body)))

(defclass object ()
  ((color :type (member white grey black)
          :accessor color
          :initform 'white)
   (index :type (integer 0 *)
          :accessor index)))

(defmethod initialize-instance :after ((instance object) &key)
  (setf (index instance) (vector-push-extend instance (objects *image*))))

(defgeneric sub-objects (object))

(defun collect (roots)
  (let ((grey-list nil))
    (flet ((mark-grey (objects)
             (dolist (object objects)
               (when (typep object 'object)
                 (when (eq (color object) 'white)
                   (setf (color object) 'grey)
                   (cl:push object grey-list)))))
           (mark-black (object)
             (setf (color object) 'black)))
      ;; Mark roots grey
      (mark-grey roots)
      ;; Scan sub-objects then mark black
      (do ((object (pop grey-list) (pop grey-list)))
          ((null object))
        (mark-grey (sub-objects object))
        (mark-black object))
      ;; Compact
      (with-slots (objects) *image*
        (do* ((source 0 (1+ source))
              (object (aref objects source) (aref objects source))
              (destination 0))
             ((= source (length objects))
              (setf (fill-pointer objects) destination))
          (ecase (color object)
            (white)
            (black
             (setf (aref objects destination) object)
             (setf (index object) destination)
             (setf (color object) 'white)
             (incf destination))))))))

(defclass cons (object)
  ((car :accessor car :initarg :car)
   (cdr :accessor cdr :initarg :cdr)))

(defun cons (car cdr)
  (make-instance 'cons :car car :cdr cdr))

(defmethod sub-objects ((object cons))
  (list (car object) (cdr object)))

(defmacro push (obj place &environment env)
  (multiple-value-bind (vars vals store-vars set get)
      (get-setf-expansion place env)
    (destructuring-bind (store-var) store-vars
      `(let* (,@(mapcar #'list vars vals)
              (,store-var (cons ,obj ,get)))
         ,set))))
