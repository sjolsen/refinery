(uiop:define-package :borax-virtual-machine/object-file
  (:mix :uiop/common-lisp :borax-virtual-machine/memory)
  (:export #:write-object-file))

(in-package :borax-virtual-machine/object-file)

(defclass file-allocator ()
  ((cons-chunk-size :type fixnum
                    :accessor cons-chunk-size
                    :initarg :cons-chunk-size)
   (object-chunk-size :type fixnum
                      :accessor object-chunk-size
                      :initarg :object-chunk-size)
   (translation :type (vector *)
                :reader translation
                :initarg :translation)))

(defun make-file-allocator ()
  (with-slots (memory-model objects) *image*
    (make-instance 'file-allocator
                   :cons-chunk-size (cons-first-word memory-model)
                   :object-chunk-size (object-first-word memory-model)
                   :translation (make-array (length objects)
                                            :element-type (word-type memory-model)
                                            :initial-element 0))))

(defgeneric file-allocate-object (file-allocator object))

(defmethod file-allocate-object (file-allocator (object borax-vm/memory:cons))
  (with-slots (cons-chunk-size translation) file-allocator
    (with-slots (memory-model) *image*
      (with-slots (page-bytes word-bytes cons-first-word) memory-model
        ;; Extend the chunk data up to the next cons cell
        (let ((page-words (floor page-bytes word-bytes)))
          (multiple-value-bind (page-index page-word)
              (floor cons-chunk-size page-words)
            (setf page-word (max page-word cons-first-word))
            (setf cons-chunk-size (+ (* page-index page-words) page-word))))
        ;; Allocate the cons cell
        (setf (aref translation (index object)) cons-chunk-size)
        (incf cons-chunk-size 2)))))

(defmethod file-allocate-object (file-allocator (object record))
  (with-slots (object-chunk-size translation) file-allocator
    ;; Align the chunk data for the next object
    (when (oddp object-chunk-size)
      (incf object-chunk-size))
    ;; Allocate the record
    (setf (aref translation (index object)) object-chunk-size)
    (incf object-chunk-size (+ 3 (length (record-data object))))))

(defun file-allocate ()
  (let ((file-allocator (make-file-allocator)))
    (with-slots (objects) *image*
      (loop for object across objects
            do (file-allocate-object file-allocator object)))
    file-allocator))

(defgeneric translate (file-allocator object))

(defun tag-word-index (tag word-index)
  (with-slots (memory-model) *image*
    (with-slots (word-bits word-bytes) memory-model
      (logior 1                              ;; pointer tag
              (ash tag (- word-bits 3))      ;; section tag
              (* word-index word-bytes)))))  ;; section offset

(defmethod translate (file-allocator (object borax-vm/memory:cons))
  (with-slots (translation) file-allocator
    (tag-word-index 1 (aref translation (index object)))))

(defmethod translate (file-allocator (object record))
  (with-slots (translation) file-allocator
    (tag-word-index 2 (aref translation (index object)))))

(defmethod translate (file-allocator (object integer))
  (with-slots (memory-model) *image*
    ;; TODO: arbitrary integers
    (assert (>= object (borax-vm/memory:most-negative-fixnum memory-model)))
    (assert (<= object (borax-vm/memory:most-positive-fixnum memory-model)))
    (dpb object (byte (1- (word-bits memory-model)) 1) 0)))

(defun write-file (file-allocator root stream)
  (with-slots (cons-chunk-size object-chunk-size translation) file-allocator
    (with-slots (memory-model objects) *image*
      (with-slots (word-bits word-bytes) memory-model
        (let* ((halfword-bits (/ word-bits 2))
               (header-bytes (+ 8 word-bytes (* 6 3 word-bytes)))
               (cons-offset header-bytes)
               (cons-size (* word-bytes cons-chunk-size))
               (object-offset (+ cons-offset cons-size))
               (object-size (* word-bytes object-chunk-size)))
          (labels ((pack-halfwords (low high)
                     (logior low (ash high halfword-bits)))
                   (write-word (word)
                     (do ((i 0 (+ i 8)))
                         ((>= i word-bits))
                       (write-byte (ldb (byte 8 i) word) stream)))
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
            (write-byte (ecase word-bits
                          (32 1)
                          (64 2))
                        stream)
            ;; Padding
            (write-byte 0 stream)
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
                  when (typep object 'borax-vm/memory:cons)
                    do (let ((word-index (aref translation (index object))))
                         (advance-to (+ cons-offset (* word-bytes word-index)))
                         (write-translation (borax-vm/memory:car object))
                         (write-translation (borax-vm/memory:cdr object))))
            (advance-to (+ cons-offset cons-size))
            ;; Object data
            (loop for object across objects
                  when (typep object 'record)
                    do (let ((word-index (aref translation (index object))))
                         (advance-to (+ object-offset (* word-bytes word-index)))
                         (write-word (pack-halfwords (record-widetag object)
                                                     (record-length-aux object)))
                         (write-word (length (record-data object)))
                         (write-translation (record-class object))
                         (etypecase object
                           (word-record (loop for datum across (record-data object)
                                              do (write-word datum)))
                           (object-record (loop for datum across (record-data object)
                                                do (write-translation datum))))))
            (advance-to (+ object-offset object-size))))))))

(defun write-object-file (root stream)
  (write-file (file-allocate) root stream))
