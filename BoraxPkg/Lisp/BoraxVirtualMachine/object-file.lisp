(uiop:define-package :borax-virtual-machine/object-file
  (:use :uiop/common-lisp :borax-virtual-machine/memory)
  (:export #:write-object-file))

(in-package :borax-virtual-machine/object-file)

(defclass file-allocator ()
  ((objects :type (vector *)
            :reader objects
            :initarg :objects)
   (memory-model :type memory-model
                 :reader memory-model
                 :initarg :memory-model)
   (cons-chunk-size :type fixnum
                    :accessor cons-chunk-size
                    :initarg :cons-chunk-size)
   (object-chunk-size :type fixnum
                      :accessor object-chunk-size
                      :initarg :object-chunk-size)
   (translation :type (vector *)
                :reader translation
                :initarg :translation)))

(defun make-file-allocator (allocator memory-model)
  (make-instance 'file-allocator
                 :objects (objects allocator)
                 :memory-model memory-model
                 :cons-chunk-size (cons-first-word memory-model)
                 :object-chunk-size (object-first-word memory-model)
                 :translation (make-array (length (objects allocator))
                                          :element-type (word-type memory-model)
                                          :initial-element 0)))

(defgeneric file-allocate-object (file-allocator object))

(defmethod file-allocate-object (file-allocator (object bx-cons))
  (with-slots (memory-model cons-chunk-size translation) file-allocator
    ;; Extend the chunk data up to the next cons cell
    (let ((page-words (floor +page-bytes+ (word-bytes memory-model))))
      (multiple-value-bind (page-index page-word)
          (floor cons-chunk-size page-words)
        (setf page-word (max page-word (cons-first-word memory-model)))
        (setf cons-chunk-size (+ (* page-index page-words) page-word))))
    ;; Allocate the cons cell
    (setf (aref translation (index object)) cons-chunk-size)
    (incf cons-chunk-size 2)))

(defmethod file-allocate-object (file-allocator (object record))
  (with-slots (memory-model object-chunk-size translation) file-allocator
    ;; Align the chunk data for the next object
    (when (oddp object-chunk-size)
      (incf object-chunk-size))
    ;; Allocate the record
    (setf (aref translation (index object)) object-chunk-size)
    (incf object-chunk-size (+ 3 (length (record-data object))))))

(defun file-allocate (allocator memory-model)
  (let ((file-allocator (make-file-allocator allocator memory-model)))
    (loop for object across (objects allocator)
          do (file-allocate-object file-allocator object))
    file-allocator))

(defgeneric translate (file-allocator object))

(defun tag-word-index (memory-model tag word-index)
  (logior 1  ;; pointer tag
          (ash tag (- (word-bits memory-model) 3))
          (* word-index (word-bytes memory-model))))

(defmethod translate (file-allocator (object bx-cons))
  (with-slots (memory-model translation) file-allocator
    (tag-word-index memory-model 1 (aref translation (index object)))))

(defmethod translate (file-allocator (object record))
  (with-slots (memory-model translation) file-allocator
    (tag-word-index memory-model 2 (aref translation (index object)))))

(defmethod translate (file-allocator (object integer))
  (with-slots (memory-model) file-allocator
    ;; TODO: arbitrary integers
    (assert (>= object (most-negative-bx-fixnum memory-model)))
    (assert (<= object (most-positive-bx-fixnum memory-model)))
    (dpb object (byte (1- (word-bits memory-model)) 1) 0)))

(defun write-word-little-endian (word-bits word stream)
  (do ((i 0 (+ i 8)))
      ((>= i word-bits))
    (write-byte (ldb (byte 8 i) word) stream)))

(defun write-word-big-endian (word-bits word stream)
  (do ((i (- word-bits 8) (- i 8)))
      ((< i 0))
    (write-byte (ldb (byte 8 i) word) stream)))

(defun write-file (file-allocator root stream)
  (with-slots (objects memory-model
               cons-chunk-size object-chunk-size
               translation)
      file-allocator
    (let* ((write-word (ecase (endianness memory-model)
                         (:little-endian #'write-word-little-endian)
                         (:big-endian #'write-word-big-endian)))
           (word-bits (word-bits memory-model))
           (word-bytes (word-bytes memory-model))
           (header-bytes (+ 8 word-bytes (* 6 3 word-bytes)))
           (cons-offset header-bytes)
           (cons-size (* word-bytes cons-chunk-size))
           (object-offset (+ cons-offset cons-size))
           (object-size (* word-bytes object-chunk-size)))
      (labels ((write-word (word)
                 (funcall write-word word-bits word stream))
               (write-section (offset size)
                 (write-word offset)
                 (write-word size)
                 ;; relocation count
                 (write-word 0))
               (advance-to (offset)
                 (let ((remainder (- offset (file-position stream))))
                   (assert (>= remainder 0))
                   (dotimes (n remainder)
                     (write-byte 0 stream))))
               (write-translation (object)
                 (write-word (translate file-allocator object))))
        ;; \x7f B X O
        (write-byte #x7f stream)
        (write-byte (char-code #\B) stream)
        (write-byte (char-code #\X) stream)
        (write-byte (char-code #\O) stream)
        ;; Word size
        (write-byte (ecase (word-bits memory-model)
                      (32 1)
                      (64 2))
                    stream)
        ;; Endianness
        (write-byte (ecase (endianness memory-model)
                      (:little-endian 1)
                      (:big-endian    2))
                    stream)
        ;; Version
        (write-byte 0 stream)
        ;; Padding
        (write-byte 0 stream)
        ;; Root object
        (write-word (translate file-allocator root))
        ;; Sections
        (write-section cons-offset cons-size)
        (write-section object-offset object-size)
        (write-section 0 0)  ; string
        (write-section 0 0)  ; package
        (write-section 0 0)  ; symbol
        (write-section 0 0)  ; class
        ;; Cons data
        (loop for object across objects
              when (typep object 'bx-cons)
                do (let ((word-index (aref translation (index object))))
                     (advance-to (+ cons-offset (* word-bytes word-index)))
                     (write-translation (bx-car object))
                     (write-translation (bx-cdr object))))
        (advance-to (+ cons-offset cons-size))
        ;; Object data
        (loop for object across objects
              when (typep object 'record)
                do (let ((word-index (aref translation (index object))))
                     (advance-to (+ object-offset (* word-bytes word-index)))
                     (write-word (ecase (record-widetag object)
                                   (:word-record   #x03)
                                   (:object-record #x07)))
                     (write-word (length (record-data object)))
                     (write-translation (record-class object))
                     (ecase (record-widetag object)
                       (:word-record (loop for datum across (record-data object)
                                           do (write-word datum)))
                       (:object-record (loop for datum across (record-data object)
                                             do (write-translation datum))))))
        (advance-to (+ object-offset object-size))))))

(defun write-object-file (allocator memory-model root stream)
  (let ((file-allocator (file-allocate allocator memory-model)))
    (write-file file-allocator root stream)))
