(uiop:define-package :borax-build/c-testing
  (:use :uiop/common-lisp :borax-build/workspace
        :borax-virtual-machine/memory :borax-virtual-machine/object-file)
  (:export #:make-test-files))

(in-package :borax-build/c-testing)

(defun make-test-file (path memory-model)
  "Generate test inputs for BoraxVirtualMachineTest.cpp"
  (with-open-file (stream path :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede
                               :if-does-not-exist :create)
    (let* ((*allocator* (make-allocator))
           (root-class (make-record :object-record nil #()))
           (root-data (make-array 6 :initial-element 0))
           (root (make-record :object-record root-class root-data)))
      ;; root-class is an instance of itself, like standard-class
      (setf (record-class root-class) root-class)
      ;; root[0] is a circular reference back to the root
      (setf (aref root-data 0) root)
      ;; root[1] is an improper list (4 3 2 1 . 0)
      (let ((head 0))
        (dotimes (i 4) (bx-push (1+ i) head))
        (setf (aref root-data 1) head))
      ;; root[2] is a circular list #1=(8 7 6 5 . #1#)
      (let* ((head (bx-cons 5 nil))
             (tail head))
        (dotimes (i 3) (bx-push (+ 6 i) head))
        (setf (bx-cdr tail) head)
        (setf (aref root-data 2) head))
      ;; root[3] is an object vector containing fixnums
      (setf (aref root-data 3)
            (make-record :object-record root-class #(343 8675309 -9000)))
      ;; root[4] is a word vector
      (setf (aref root-data 4)
            (make-record :word-record root-class #(343 8675309 -9000)))
      ;; root[5] is a byte vector #(0 1 2)
      (setf (aref root-data 5)
            (make-record :word-record root-class #(#x020100)
                         :length-aux (- (word-bytes memory-model) 3)))
      (write-object-file *allocator* memory-model root stream))))

(defun make-test-files ()
  (let* ((test-base (uiop:merge-pathnames* #P"BoraxPkg/Test/BoraxVirtualMachineTest/" *refinery-root*))
         (test-files `((#P"TestFileIA32.bxo" . ,(make-memory-model 32))
                       (#P"TestFileX64.bxo"  . ,(make-memory-model 64)))))
    (loop for (basename . memory-model) in test-files
          for path = (uiop:merge-pathnames* basename test-base)
          do (make-test-file path memory-model))))
