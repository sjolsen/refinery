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

(defun test ()
  (run-suite 'collect-suite))
