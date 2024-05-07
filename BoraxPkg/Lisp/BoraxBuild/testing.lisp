(uiop:define-package :borax-build/testing
  (:use :uiop/common-lisp :uiop :clunit :borax-build/asdf-extensions)
  (:export #:test-failure #:suite-report #:run-tests))

(in-package :borax-build/testing)

(define-condition test-failure (error)
  ((suite-report :type list
                 :reader suite-report
                 :initarg :suite-report))
  (:report (lambda (c stream)
             (format stream "~A" (suite-report c)))))

(defun get-test-suites (component)
  (let* ((packages (make-hash-table)))
    (dolist (dep (system-tests component))
      (setf (gethash (find-package* dep) packages) t))
    (loop :for suite :in (get-defined-suites)
          :when (gethash (symbol-package suite) packages)
            :collect suite)))

(defun get-test-failures (suite-report)
  (loop :for test-report :in (test-reports suite-report)
        :when (not (test-report-passed-p test-report))
          :collect test-report))

(defun run-tests (component)
  (loop :for suite :in (get-test-suites component)
        :for suite-report := (run-suite suite)
        :when (get-test-failures suite-report)
          :do (progn
                (print suite-report)
                (cerror "Ignore test failure." 'test-failure
                        :suite-report suite-report))))
