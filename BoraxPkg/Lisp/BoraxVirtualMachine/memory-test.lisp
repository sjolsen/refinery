(uiop:define-package :borax-virtual-machine/memory-test
  (:use :uiop/common-lisp :borax-virtual-machine/memory :clunit))

(in-package :borax-virtual-machine/memory-test)

(defsuite memory-model-suite ())

(deftest test-32bit (memory-model-suite)
  (let ((memory-model (make-memory-model 32)))
    (assert-equal 32 (word-bits memory-model))
    (assert-equal 18 (cons-first-word memory-model))
    (assert-equal #x3fffffff (most-positive-bx-fixnum memory-model))
    (assert-equal #x-40000000 (most-negative-bx-fixnum memory-model))))

(deftest test-64bit (memory-model-suite)
  (let ((memory-model (make-memory-model 64)))
    (assert-equal 64 (word-bits memory-model))
    (assert-equal 6 (cons-first-word memory-model))
    (assert-equal #x3fffffffffffffff (most-positive-bx-fixnum memory-model))
    (assert-equal #x-4000000000000000 (most-negative-bx-fixnum memory-model))))

(defsuite collect-suite ())

(defmacro with-allocator (name &body body)
  `(let* ((,name (make-allocator))
          (*allocator* ,name))
     ,@body))

(deftest test-cons-discard (collect-suite)
  (with-allocator a
    (bx-cons 1 2)
    (collect nil)
    (assert-equal 0 (length (objects a)))))

(deftest test-cons-keep (collect-suite)
  (with-allocator a
    (collect (list (bx-cons 1 2)))
    (assert-equal 1 (length (objects a)))))

(deftest test-cons-keep-some (collect-suite)
  (with-allocator a
    (let ((roots nil))
      (dotimes (n 5)
        (bx-cons 1 2)
        (push (bx-cons 3 4) roots))
      (collect roots))
    (assert-equal 5 (length (objects a)))
    (loop for i upfrom 0
          for object across (objects a)
          do (assert-equal i (index object)))))

(defun make-circular (n)
  (let* ((last (bx-cons 0 nil))
         (first last))
    (dotimes (i n)
      (setf first (bx-cons (1+ i) first)))
    (setf (bx-cdr last) first)))

(deftest test-circular-discard (collect-suite)
  (with-allocator a
    (make-circular 5)
    (collect nil)
    (assert-equal 0 (length (objects a)))))

(deftest test-circular-keep (collect-suite)
  (with-allocator a
    (collect (list (make-circular 5)))
    (assert-equal 6 (length (objects a)))))

(defun make-funny-record ()
  (let ((class (make-record :word-record nil #())))
    (setf (record-class class) class)
    (let* ((nested (make-record :word-record class #(1 2 3)))
           (data (make-array 3 :initial-contents (list nested nested nil))))
      (make-record :object-record class data))))

(deftest test-record-discard (collect-suite)
  (with-allocator a
    (make-funny-record)
    (collect nil)
    (assert-equal 0 (length (objects a)))))

(deftest test-record-keep (collect-suite)
  (with-allocator a
    (collect (list (make-funny-record)))
    (assert-equal 3 (length (objects a)))))
