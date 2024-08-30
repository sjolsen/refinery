(uiop:define-package :borax-virtual-machine/object-file
  (:mix :closer-mop :uiop/common-lisp)
  (:use :borax-virtual-machine/image)
  (:export ;; Allocation protocol
           #:cons-section #:object-section
           #:get-object-section #:allocate-in-section
           ;; Stream output protocol
           #:write-word #:write-halfword #:write-translation #:write-object
           ;; Object file API
           #:write-object-file))

(in-package :borax-virtual-machine/object-file)

(defclass section ()
  ((cursor :type fixnum
           :accessor cursor
           :initarg :cursor)
   (base :type fixnum
         :accessor base
         :initform 0)))

(defun size (section)
  (* (word-bytes (memory-model *image*))
     (cursor section)))

(defclass cons-section (section) ())

(defun make-cons-section ()
  (with-slots (memory-model) *image*
    (make-instance 'cons-section :cursor (cons-first-word memory-model))))

(defclass object-section (section) ())

(defun make-object-section ()
  (with-slots (memory-model) *image*
    (make-instance 'object-section :cursor (object-first-word memory-model))))

(defclass allocator ()
  ((cons-section :type cons-section
                 :reader cons-section
                 :initform (make-cons-section))
   (object-section :type object-section
                   :reader object-section
                   :initform (make-object-section))
   (offsets :type (vector *)
            :reader offsets
            :initarg :offsets)))

(defvar *allocator* nil)

(defun make-allocator ()
  (with-slots (memory-model objects) *image*
    (make-instance 'allocator
                   :offsets (make-array (length objects)
                                        :element-type (word-type memory-model)
                                        :initial-element 0))))

(defun object-offset (object)
  (aref (offsets *allocator*) (index object)))

(defun (setf object-offset) (offset object)
  (setf (aref (offsets *allocator*) (index object)) offset))

(defgeneric get-object-section (object))

(defmethod get-object-section ((object borax-vm/cl:cons))
  (values 'cons-section))

(defmethod get-object-section ((object record-object))
  (let ((class (class-of object)))
    (values 'object-section :size (+ 3 (length (record-slots class))))))

(defmethod get-object-section ((object record-vector))
  (values 'object-section :size (+ 3 (length (vector-data object)))))

(defgeneric allocate-in-section (section object &key &allow-other-keys))

(defmethod allocate-in-section ((section cons-section) (object borax-vm/cl:cons) &key &allow-other-keys)
  (with-slots (memory-model) *image*
    (with-slots (page-bytes word-bytes cons-first-word) memory-model
      (with-slots (cursor) section
        ;; Extend the chunk data up to the next cons cell
        (let ((page-words (floor page-bytes word-bytes)))
          (multiple-value-bind (page-index page-word)
              (floor cursor page-words)
            (setf page-word (max page-word cons-first-word))
            (setf cursor (+ (* page-index page-words) page-word))))
        ;; Allocate the cons cell
        (prog1 cursor
          (incf cursor 2))))))

(defmethod allocate-in-section ((section object-section) object &key size &allow-other-keys)
  (with-slots (cursor) section
    ;; Align the chunk data for the next object
    (when (oddp cursor)
      (incf cursor))
    ;; Allocate the object
    (prog1 cursor
      (incf cursor size))))

(defun allocate (object)
  (destructuring-bind (section-name . rest)
      (multiple-value-list (get-object-section object))
    (let* ((section (ecase section-name
                      (cons-section (cons-section *allocator*))
                      (object-section (object-section *allocator*))))
           (offset (apply #'allocate-in-section section object rest)))
      (setf (object-offset object) offset))))

(defgeneric translate (object))

(defmethod translate ((object object))
  (let ((word-index (object-offset object))
        (tag (ecase (get-object-section object)
               (cons-section 1)
               (object-section 2))))
    (with-slots (memory-model) *image*
      (with-slots (word-bits word-bytes) memory-model
        (logior 1                               ;; pointer tag
                (ash tag (- word-bits 3))       ;; section tag
                (* word-index word-bytes))))))  ;; section offset

(defmethod translate ((object integer))
  (with-slots (memory-model) *image*
    ;; TODO: arbitrary integers
    (assert (>= object (borax-vm/cl:most-negative-fixnum memory-model)))
    (assert (<= object (borax-vm/cl:most-positive-fixnum memory-model)))
    (ash object 1)))

(defmethod translate ((object character))
  (logior #x7FD (ash (char-code object) 11)))

(defvar *stream* nil)

(defun advance-to (offset)
  (let ((remainder (- offset (file-position *stream*))))
    (assert (>= remainder 0))
    (dotimes (n remainder)
      (write-byte 0 *stream*))))

(defun write-integer (word bytes)
  (let ((bits (* 8 bytes)))
    (do ((i 0 (+ i 8)))
        ((>= i bits))
      (write-byte (ldb (byte 8 i) word) *stream*))))

(defun write-halfword (word)
  (write-integer word (floor (word-bytes (memory-model *image*)) 2)))

(defun write-word (word)
  (write-integer word (word-bytes (memory-model *image*))))

(defun write-translation (object)
  (write-word (translate object)))

(defgeneric write-section-header (section))

(defmethod write-section-header ((section null))
  (write-word 0)
  (write-word 0)
  (write-word 0))

(defmethod write-section-header ((section section))
  (write-word (base section))
  (write-word (size section))
  (write-word 0))  ; relocation count

(defun write-section-data (section)
  (with-slots (memory-model) *image*
    (with-slots (word-bytes) memory-model
      (let ((section-name (class-name (class-of section)))
            (base (base section))
            (size (size section)))
        (loop for object across (objects *image*)
              when (eq section-name (get-object-section object))
                do (let ((word-index (object-offset object)))
                     (advance-to (+ base (* word-bytes word-index)))
                     (write-object object)))
        (advance-to (+ base size))))))

(defgeneric write-object (object))

(defmethod write-object ((object borax-vm/cl:cons))
  (write-translation (borax-vm/cl:car object))
  (write-translation (borax-vm/cl:cdr object)))

(defmethod write-object ((object record-object))
  (let ((class (class-of object)))
    (write-word #x07)  ; widetag object-record
    (write-word (length (record-slots class)))
    (write-word #x07)  ; unbound
    (do-record-slots (value) object
      (write-translation value))))

(defmethod write-object ((object record-vector))
  (write-word #x07)  ; widetag object-record
  (write-word (length (vector-data object)))
  (write-word #x07)  ; unbound
  (map nil #'write-translation (vector-data object)))

(defun write-file (root)
  (with-slots (cons-section object-section) *allocator*
    (with-slots (memory-model) *image*
      (with-slots (word-bits word-bytes) memory-model
        (let* ((header-bytes (+ 8 word-bytes (* 6 3 word-bytes)))
               (file-cursor header-bytes))
          (flet ((allocate-section (section)
                   (setf (base section) file-cursor)
                   (incf file-cursor (* word-bytes (cursor section)))))
            (allocate-section cons-section)
            (allocate-section object-section)))
        ;; \x7f B X O
        (write-byte #x7f *stream*)
        (write-byte (char-code #\B) *stream*)
        (write-byte (char-code #\X) *stream*)
        (write-byte (char-code #\O) *stream*)
        ;; Word size
        (write-byte (ecase word-bits
                      (32 1)
                      (64 2))
                    *stream*)
        ;; Padding
        (write-byte 0 *stream*)
        ;; Version
        (write-byte 0 *stream*)
        ;; Padding
        (write-byte 0 *stream*)
        ;; Root object
        (write-translation root)
        ;; Section headers
        (write-section-header cons-section)
        (write-section-header object-section)
        (write-section-header nil)  ; string
        (write-section-header nil)  ; package
        (write-section-header nil)  ; symbol
        (write-section-header nil)  ; class
        ;; Section data
        (write-section-data cons-section)
        (write-section-data object-section)))))

(defun write-object-file (root *stream*)
  (let ((*allocator* (make-allocator)))
    (loop for object across (objects *image*)
          do (allocate object))
    (write-file root)))
