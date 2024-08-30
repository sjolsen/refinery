(uiop:define-package :borax-virtual-machine/mock-record
  (:use :uiop/common-lisp :clunit :cl-locatives :flexi-streams
        :borax-virtual-machine/image
        :borax-virtual-machine/object-file)
  (:export #:record #:object-record #:word-record
           #:make-object-record #:make-word-record
           #:widetag #:length-aux #:record-class #:record-data #:data))

(in-package :borax-virtual-machine/mock-record)

(defclass record (object)
  ((class :type record
          :accessor record-class
          :initarg :class)
   (data :type vector
         :accessor record-data
         :initarg :data)))

(defmethod sub-objects ((object record))
  (list* (locative-for (record-class object))
         (loop with record-data = (record-data object)
               for i from 0 below (length record-data)
               collect (locative-for (aref record-data i)))))

(defclass object-record (record)
  ((widetag :type fixnum
            :reader widetag
            :allocation :class
            :initform #x07)))

(defun make-object-record (class data)
  (make-instance 'object-record :class class :data data))

(defclass word-record (record)
  ((widetag :type fixnum
            :reader widetag
            :allocation :class
            :initform #x03)
   (length-aux :type fixnum
               :accessor length-aux
               :initarg :length-aux
               :initform 0)))

(defun make-word-record (class data &key (length-aux 0))
  (make-instance 'word-record :class class :data data :length-aux length-aux))

(defmethod get-object-section ((object record))
  (values 'object-section :size (+ 3 (length (record-data object)))))

(defmethod write-object ((object object-record))
  (with-slots (widetag class data) object
    (write-word widetag)
    (write-word (length data))
    (write-translation class)
    (loop for datum across data
          do (write-translation datum))))

(defmethod write-object ((object word-record))
  (with-slots (widetag class data length-aux) object
    (write-halfword widetag)
    (write-halfword length-aux)
    (write-word (length data))
    (write-translation class)
    (loop for datum across data
          do (write-word datum))))
