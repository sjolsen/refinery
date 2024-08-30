(uiop:define-package :borax-virtual-machine/image-test
  (:use :uiop/common-lisp :clunit :cl-locatives
        :borax-virtual-machine/image
        :borax-virtual-machine/mock-record))

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

(defsuite collect-suite ())

(deftest test-cons-discard (collect-suite)
  (with-image nil
    (borax-vm/cl:cons 1 2)
    (reify-image)
    (assert-equal 0 (length (objects *image*)))))

(deftest test-cons-keep (collect-suite)
  (with-image nil
    (setf (root *image*) (borax-vm/cl:cons 1 2))
    (reify-image)
    (assert-equal 1 (length (objects *image*)))))

(deftest test-cons-keep-some (collect-suite)
  (with-image nil
    (with-slots (root) *image*
      (dotimes (n 5)
        (borax-vm/cl:cons 1 2)
        (push (borax-vm/cl:cons 3 4) root))
      (reify-image))
    (assert-equal 10 (length (objects *image*)))
    (loop for i upfrom 0
          for object across (objects *image*)
          do (assert-equal i (index object)))))

(defun make-circular (n)
  (let* ((last (borax-vm/cl:cons 0 +unbound+))
         (first last))
    (dotimes (i n)
      (setf first (borax-vm/cl:cons (1+ i) first)))
    (setf (borax-vm/cl:cdr last) first)))

(deftest test-circular-discard (collect-suite)
  (with-image nil
    (make-circular 5)
    (reify-image)
    (assert-equal 0 (length (objects *image*)))))

(deftest test-circular-keep (collect-suite)
  (with-image nil
    (setf (root *image*) (make-circular 5))
    (reify-image)
    (assert-equal 6 (length (objects *image*)))))

(defun make-funny-record ()
  (let ((class (make-word-record +unbound+ #())))
    (setf (record-class class) class)
    (let* ((nested (make-word-record class #(1 2 3)))
           (data (make-array 3 :initial-contents (list nested nested +unbound+))))
      (make-object-record class data))))

(deftest test-record-discard (collect-suite)
  (with-image nil
    (make-funny-record)
    (reify-image)
    (assert-equal 0 (length (objects *image*)))))

(deftest test-record-keep (collect-suite)
  (with-image nil
    (setf (root *image*) (make-funny-record))
    (reify-image)
    (assert-equal 3 (length (objects *image*)))))

(defsuite record-object-suite ())

(defclass test ()
  ((x :initform 42)
   (y :initform t)
   (z :initform "hello"))
  (:metaclass record-class))

(deftest test-basic-object (record-object-suite)
  (with-image nil
    (let ((o (make-instance 'test)))
      (assert-equal (list (find-class 'test) 42 t "hello")
          (mapcar #'dereference (sub-objects o))))))

(defclass test2 (test)
  ((a :initform #\A))
  (:metaclass record-class))

(deftest test-derived-object (record-object-suite)
  (with-image nil
    (let ((o (make-instance 'test2)))
      (assert-equal (list (find-class 'test2) 42 t "hello" #\A)
          (mapcar #'dereference (sub-objects o))))))
