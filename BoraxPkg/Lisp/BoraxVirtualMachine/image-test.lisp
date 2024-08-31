(uiop:define-package :borax-virtual-machine/image-test
  (:use :uiop/common-lisp :clunit :cl-locatives
        :borax-virtual-machine/image))

(in-package :borax-virtual-machine/image-test)

(defsuite memory-model-suite ())

(deftest test-32bit (memory-model-suite)
  (assert-equal 32 (word-bits +32-bit+))
  (assert-equal 18 (cons-first-word +32-bit+))
  (assert-equal #x3fffffff (borax-vm/cl:most-positive-fixnum +32-bit+))
  (assert-equal #x-40000000 (borax-vm/cl:most-negative-fixnum +32-bit+)))

(deftest test-64bit (memory-model-suite)
  (assert-equal 64 (word-bits +64-bit+))
  (assert-equal 6 (cons-first-word +64-bit+))
  (assert-equal #x3fffffffffffffff (borax-vm/cl:most-positive-fixnum +64-bit+))
  (assert-equal #x-4000000000000000 (borax-vm/cl:most-negative-fixnum +64-bit+)))

(defsuite record-object-suite ())

(defclass test ()
  ((x :initform 42)
   (y :initform t)
   (z :initform "hello"))
  (:metaclass record-class))

(deftest test-basic-object (record-object-suite)
  (with-image nil
    (let ((o (make-instance 'test)))
      (assert-equal '(42 t "hello")
          (mapcar #'dereference (cdr (sub-objects o)))))))

(defclass test2 (test)
  ((a :initform #\A))
  (:metaclass record-class))

(deftest test-derived-object (record-object-suite)
  (with-image nil
    (let ((o (make-instance 'test2)))
      (assert-equal '(42 t "hello" #\A)
          (mapcar #'dereference (cdr (sub-objects o)))))))
