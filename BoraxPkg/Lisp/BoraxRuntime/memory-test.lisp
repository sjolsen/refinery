(uiop:define-package :borax-runtime/memory-test
  (:mix :borax-runtime/memory :uiop/common-lisp)
  (:use :clunit)
  (:export #:test))

(in-package :borax-runtime/memory-test)

(defsuite collect-suite ())

(defmacro with-allocator (name &body body)
  `(let* ((,name (make-allocator))
          (*allocator* ,name))
     ,@body))

(deftest test-cons-discard (collect-suite)
  (with-allocator a
    (cons 1 2)
    (collect nil)
    (assert-equal 0 (length (objects a)))))

(deftest test-cons-keep (collect-suite)
  (with-allocator a
    (collect (list (cons 1 2)))
    (assert-equal 1 (length (objects a)))))

(deftest test-cons-keep-some (collect-suite)
  (with-allocator a
    (let ((roots nil))
      (dotimes (n 5)
        (cons 1 2)
        (push (cons 3 4) roots))
      (collect roots))
    (assert-equal 5 (length (objects a)))
    (loop for i upfrom 0
          for object across (objects a)
          do (assert-equal i (index object)))))

(defun make-circular (n)
  (let* ((last (cons 0 nil))
         (first last))
    (dotimes (i n)
      (setf first (cons (1+ i) first)))
    (setf (cdr last) first)))

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
  (let ((class (make-record nil #())))
    (setf (record-class class) class)
    (let* ((nested (make-record class #(1 2 3)))
           (data (make-array 3 :initial-contents (list nested nested nil))))
      (make-record class data))))

(deftest test-record-discard (collect-suite)
  (with-allocator a
    (make-funny-record)
    (collect nil)
    (assert-equal 0 (length (objects a)))))

(deftest test-record-keep (collect-suite)
  (with-allocator a
    (collect (list (make-funny-record)))
    (assert-equal 3 (length (objects a)))))
