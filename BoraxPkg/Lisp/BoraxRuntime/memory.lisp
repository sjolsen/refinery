(uiop:define-package :borax-runtime/memory
  (:use :uiop/common-lisp)
  (:export #:memory-model
           #:allocator #:make-allocator #:*allocator* #:objects
           #:collect
           #:bx-cons #:bx-car #:bx-cdr
           #:object #:index
           #:record #:make-record #:record-class #:record-data))

(in-package :borax-runtime/memory)

(defclass memory-model ()
  ((word-size :type (member 32 64)
              :reader word-size
              :initarg :word-size)
   (endianness :type (member :little-endian :big-endian)
               :reader endianness
               :initarg :endianness)))

(defun memory-model (word-size endianness)
  (make-instance 'memory-model :word-size word-size :endianness endianness))

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
