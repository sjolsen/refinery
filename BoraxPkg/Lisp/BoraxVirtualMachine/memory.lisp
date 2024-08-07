(uiop:define-package :borax-virtual-machine/memory
  (:use :uiop/common-lisp)
  (:export #:+page-bytes+ #:memory-model #:make-memory-model
           #:word-bits #:cons-first-word #:object-first-word
           #:word-type #:word-bytes
           #:most-positive-bx-fixnum #:most-negative-bx-fixnum
           #:allocator #:make-allocator #:*allocator* #:objects
           #:collect
           #:bx-cons #:bx-car #:bx-cdr #:bx-push
           #:object #:index
           #:record #:object-record #:word-record
           #:make-object-record #:make-word-record
           #:record-widetag #:record-length-aux #:record-class #:record-data))

(in-package :borax-virtual-machine/memory)

(defconstant +page-bytes+ 4096)

(defclass memory-model ()
  ((word-bits :type (member 32 64)
              :reader word-bits
              :initarg :word-bits)
   (cons-first-word :type fixnum
                    :reader cons-first-word
                    :initarg :cons-first-word)
   (object-first-word :type fixnum
                      :reader object-first-word
                      :initarg :object-first-word)))

(defun make-memory-model (word-bits)
  (let* ((word-bytes (floor word-bits 8))
         (cons-bytes (* 2 word-bytes))
         (cons-per-page (floor +page-bytes+ cons-bytes))
         (cons-bitmap-words (floor cons-per-page word-bits))
         (cons-header-words (+ 2 cons-bitmap-words)))
    (make-instance 'memory-model
     :word-bits word-bits
     :cons-first-word cons-header-words
     :object-first-word 2)))

(defun word-type (memory-model)
  `(unsigned-byte ,(word-bits memory-model)))

(defun word-bytes (memory-model)
  (floor (word-bits memory-model) 8))

(defun most-positive-bx-fixnum (memory-model)
  (- (ash 1 (- (word-bits memory-model) 2)) 1))

(defun most-negative-bx-fixnum (memory-model)
  (- (ash 1 (- (word-bits memory-model) 2))))

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

(defclass bx-cons (object)
  ((bx-car :accessor bx-car :initarg :car)
   (bx-cdr :accessor bx-cdr :initarg :cdr)))

(defun bx-cons (car cdr)
  (make-instance 'bx-cons :car car :cdr cdr))

(defmethod sub-objects ((object bx-cons))
  (list (bx-car object) (bx-cdr object)))

(defmacro bx-push (obj place &environment env)
  (multiple-value-bind (vars vals store-vars set get)
      (get-setf-expansion place env)
    (destructuring-bind (store-var) store-vars
      `(let* (,@(mapcar #'list vars vals)
              (,store-var (bx-cons ,obj ,get)))
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
                   (push object grey-list)))))
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
