(uiop:define-package :borax-build/asdf-extensions
  (:use :uiop/common-lisp :asdf :uiop)
  (:export #:package-inferred-system-with-tests #:system-tests))

(in-package :borax-build/asdf-extensions)

(defclass package-inferred-system-with-tests (package-inferred-system)
  ((tests :type list
          :accessor system-tests
          :initarg :tests)))

(defmethod component-depends-on ((operation test-op) (component package-inferred-system-with-tests))
  (flet ((make-load-op (dep)
           `(load-op ,dep)))
    (nconc (mapcar #'make-load-op (system-tests component))
           (list (make-load-op :borax-build/lisp-testing))
           (call-next-method))))

(defmethod perform ((operation test-op) (component package-inferred-system-with-tests))
  (symbol-call :borax-build/lisp-testing :run-tests component))
