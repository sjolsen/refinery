(uiop:define-package :borax-virtual-machine/memory-test
  (:mix :uiop/common-lisp :borax-virtual-machine/memory)
  (:use :clunit))

(in-package :borax-virtual-machine/memory-test)

(defsuite memory-model-suite ())

(deftest test-32bit (memory-model-suite)
  (assert-equal 32 (word-bits +32-bit+))
  (assert-equal 18 (cons-first-word +32-bit+))
  (assert-equal #x3fffffff (borax-vm/memory:most-positive-fixnum +32-bit+))
  (assert-equal #x-40000000 (borax-vm/memory:most-negative-fixnum +32-bit+)))

(deftest test-64bit (memory-model-suite)
  (assert-equal 64 (word-bits +64-bit+))
  (assert-equal 6 (cons-first-word +64-bit+))
  (assert-equal #x3fffffffffffffff (borax-vm/memory:most-positive-fixnum +64-bit+))
  (assert-equal #x-4000000000000000 (borax-vm/memory:most-negative-fixnum +64-bit+)))

(defsuite collect-suite ())

(defmacro with-allocator (name &body body)
  `(let* ((,name (make-allocator))
          (*allocator* ,name))
     ,@body))

(deftest test-cons-discard (collect-suite)
  (with-allocator a
    (borax-vm/memory:cons 1 2)
    (collect nil)
    (assert-equal 0 (length (objects a)))))

(deftest test-cons-keep (collect-suite)
  (with-allocator a
    (collect (list (borax-vm/memory:cons 1 2)))
    (assert-equal 1 (length (objects a)))))

(deftest test-cons-keep-some (collect-suite)
  (with-allocator a
    (let ((roots nil))
      (dotimes (n 5)
        (borax-vm/memory:cons 1 2)
        (push (borax-vm/memory:cons 3 4) roots))
      (collect roots))
    (assert-equal 5 (length (objects a)))
    (loop for i upfrom 0
          for object across (objects a)
          do (assert-equal i (index object)))))

(defun make-circular (n)
  (let* ((last (borax-vm/memory:cons 0 nil))
         (first last))
    (dotimes (i n)
      (setf first (borax-vm/memory:cons (1+ i) first)))
    (setf (borax-vm/memory:cdr last) first)))

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
  (let ((class (make-word-record nil #())))
    (setf (record-class class) class)
    (let* ((nested (make-word-record class #(1 2 3)))
           (data (make-array 3 :initial-contents (list nested nested nil))))
      (make-object-record class data))))

(deftest test-record-discard (collect-suite)
  (with-allocator a
    (make-funny-record)
    (collect nil)
    (assert-equal 0 (length (objects a)))))

(deftest test-record-keep (collect-suite)
  (with-allocator a
    (collect (list (make-funny-record)))
    (assert-equal 3 (length (objects a)))))
