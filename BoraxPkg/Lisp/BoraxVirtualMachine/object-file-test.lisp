(uiop:define-package :borax-virtual-machine/object-file-test
  (:use :uiop/common-lisp :borax-virtual-machine/memory :borax-virtual-machine/object-file
        :clunit :flexi-streams))

(in-package :borax-virtual-machine/object-file-test)

(defun object-file-bytes (allocator memory-model root)
  (with-output-to-sequence (stream)
    (write-object-file allocator memory-model root stream)))

(defun load-word (data memory-model byte-offset)
  (let ((result 0)
        (word-bytes (word-bytes memory-model)))
    (dotimes (i word-bytes)
      (setf (ldb (byte 8 (* 8 i)) result)
            (aref data (+ byte-offset i))))
    result))

(defclass section-header ()
  ((offset :accessor section-offset :initarg :offset)
   (size :accessor section-size :initarg :size)
   (rel-count :accessor section-rel-count :initarg :rel-count)))

(defun validate-object-file (data intended-memory-model)
  ;; File identifier
  (assert-true (>= (length data) 8))
  (assert-equalp #(#x7f #x42 #x58 #x4f) (subseq data 0 4))
  (let* ((word-bits (ecase (aref data 4)
                      (1 32)
                      (2 64)))
         (memory-model (make-memory-model word-bits))
         (word-bytes (word-bytes memory-model)))
    (assert-equal (word-bits intended-memory-model) word-bits)
    (assert-equalp #(0 0) (subseq data 6 8))
    ;; Header
    (labels ((load-header-word (word-index)
               (let ((byte-offset (+ 8 (* word-bytes word-index))))
                 (load-word data memory-model byte-offset)))
             (load-section (section-index)
               (let ((section-start (+ 1 (* 3 section-index))))
                 (make-instance 'section-header
                                :offset (load-header-word section-start)
                                :size (load-header-word (+ section-start 1))
                                :rel-count (load-header-word (+ section-start 2)))))
             (validate-section (section-header)
               (with-slots (offset size rel-count) section-header
                 (assert-true (< offset (length data)))
                 (assert-true (<= (+ offset size) (length data)))
                 (assert-equal 0 rel-count))))
      ;; Root
      (assert-equal 0 (load-header-word 0))
      ;; Sections
      (dotimes (i 6)
        (validate-section (load-section i))))))

(defsuite smoke-test-suite ())

(defun smoke-test (word-bits)
  (let* ((*allocator* (make-allocator))
         (memory-model (make-memory-model word-bits))
         (data (object-file-bytes *allocator* memory-model 0)))
    (validate-object-file data memory-model)))

(deftest smoke-test-32bit (smoke-test-suite)
  (smoke-test 32))

(deftest smoke-test-64bit (smoke-test-suite)
  (smoke-test 64))
