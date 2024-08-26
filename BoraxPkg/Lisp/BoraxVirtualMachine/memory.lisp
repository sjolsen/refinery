(uiop:define-package :borax-virtual-machine/memory
  (:nicknames :borax-vm/memory)
  (:use :uiop/common-lisp)
  (:shadow #:most-positive-fixnum #:most-negative-fixnum
           #:cons #:car #:cdr #:push)
  (:export #:memory-model #:+32-bit+ #:+64-bit+
           #:page-bytes #:word-bits #:word-bytes #:word-type
           #:most-positive-fixnum #:most-negative-fixnum
           #:cons-first-word #:object-first-word
           #:allocator #:make-allocator #:*allocator* #:objects
           #:collect
           #:cons #:car #:cdr #:push
           #:object #:index
           #:record #:object-record #:word-record
           #:make-object-record #:make-word-record
           #:record-widetag #:record-length-aux #:record-class #:record-data))

(in-package :borax-virtual-machine/memory)

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

(defclass allocator ()
  ((objects :type (vector *)
            :reader objects
            :initform (make-space))))

(defun make-allocator ()
  (make-instance 'allocator))

(defvar *allocator* nil)

(defclass object ()
  ((color :type (member white grey black)
          :accessor color
          :initform 'white)
   (index :type (integer 0 *)
          :accessor index)))

(defmethod initialize-instance :after ((instance object) &key)
  (setf (index instance) (vector-push-extend instance (objects *allocator*))))

(defgeneric sub-objects (object)
  (:method ((object integer)) nil))

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

(defclass record (object)
  ((class :type record
          :accessor record-class
          :initarg :class)
   (data :type vector
         :accessor record-data
         :initarg :data)))

(defmethod sub-objects ((object record))
  (list* (record-class object)
         (concatenate 'list (record-data object))))

(defclass object-record (record)
  ())

(defun make-object-record (class data)
  (make-instance 'object-record :class class :data data))

(defclass word-record (record)
  ((length-aux :type fixnum
               :accessor record-length-aux
               :initarg :length-aux
               :initform 0)))

(defun make-word-record (class data &key (length-aux 0))
  (make-instance 'word-record :class class :data data :length-aux length-aux))

(defgeneric record-widetag (object)
  (:method ((object word-record))   #x03)
  (:method ((object object-record)) #x07))

(defmethod record-length-aux ((object object-record))
  0)

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
      (with-slots (objects) *allocator*
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
