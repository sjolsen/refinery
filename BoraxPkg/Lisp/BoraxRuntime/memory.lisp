(uiop:define-package :borax-runtime/memory
  (:use :uiop/common-lisp)
  (:export #:+page-bytes+ #:memory-model #:make-memory-model
           #:word-bits #:endianness #:alignment #:cons-first-index
           #:word-type #:align-up
           #:allocator #:make-allocator #:*allocator* #:objects
           #:collect
           #:bx-cons #:bx-car #:bx-cdr
           #:object #:index
           #:record #:make-record #:record-class #:record-data))

(in-package :borax-runtime/memory)

(defconstant +page-bytes+ 4096)

(defun align-up-bytes (bytes alignment)
  (* alignment (ceiling bytes alignment)))

(defclass memory-model ()
  ((word-bits :type (member 32 64)
              :reader word-bits
              :initarg :word-bits)
   (endianness :type (member :little-endian :big-endian)
               :reader endianness
               :initarg :endianness)
   (alignment :type fixnum
              :reader alignment
              :initarg :alignment)
   (cons-first-index :type fixnum
                     :reader cons-first-index
                     :initarg :cons-first-index)))

(defun make-memory-model (word-bits endianness)
  (let* ((word-bytes (floor word-bits 8))
         (cons-bytes (* 2 word-bytes))
         (cons-per-page (floor +page-bytes+ cons-bytes))
         (cons-bitmap-words (floor cons-per-page word-bits))
         (cons-header-words (+ 2 cons-bitmap-words))
         (cons-header-bytes (* word-bytes cons-header-words)))
    (make-instance 'memory-model
     :word-bits word-bits :endianness endianness
     :alignment cons-bytes
     :cons-first-index (align-up-bytes cons-header-bytes cons-bytes))))

(defun word-type (memory-model)
  `(unsigned-byte ,(word-bits memory-model)))

(defun align-up (bytes memory-model)
  (align-up-bytes bytes (alignment memory-model)))

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

(defclass record (object)
  ((class :type record
          :accessor record-class
          :initarg :class)
   (data :type vector
         :accessor record-data
         :initarg :data)))

(defun make-record (class data)
  (make-instance 'record :class class :data data))

(defmethod sub-objects ((object record))
  (list* (record-class object)
         (concatenate 'list (record-data object))))

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
