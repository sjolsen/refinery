(uiop:define-package :borax-build/workspace
  (:use :uiop/common-lisp)
  (:export #:*refinery-root*))

(in-package :borax-build/workspace)

(defvar *refinery-root*
  (asdf:system-relative-pathname :borax-build "../../../"))
