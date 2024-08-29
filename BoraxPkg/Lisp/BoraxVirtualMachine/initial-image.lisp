(uiop:define-package :borax-virtual-machine/initial-image
  (:mix :uiop/common-lisp :borax-virtual-machine/image)
  (:export #:make-initial-image))

(in-package :borax-virtual-machine/initial-image)

(defclass root ()
  ((numbers :initform '(0 1 2 3 4 5)))
  (:metaclass record-class))

(defun make-initial-image ()
  (make-instance 'root))
