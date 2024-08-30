(uiop:define-package :borax-virtual-machine/initial-image
  (:mix :uiop/common-lisp :borax-virtual-machine/common-lisp)
  (:use :borax-virtual-machine/image)
  (:export #:make-initial-image))

(in-package :borax-virtual-machine/initial-image)

(defclass borax-vm/cl:null ()
  ()
  (:metaclass record-class))

(defclass root ()
  (borax-vm/cl:nil
   numbers
   stuff)
  (:metaclass record-class))

(defvar *root* nil)

(defun borax-vm/cl:list (&rest values)
  (with-slots (borax-vm/cl:nil) *root*
    (let ((result borax-vm/cl:nil))
      (dolist (value (nreverse values))
        (borax-vm/cl:push value result))
      result)))

(defun borax-vm/cl:list* (&rest values)
  (with-slots (borax-vm/cl:nil) *root*
    (destructuring-bind (result . rvalues)
        (nreverse values)
      (dolist (value rvalues)
        (borax-vm/cl:push value result))
      result)))

(defun make-initial-image ()
  (let ((*root* (make-instance 'root)))
    (with-slots (borax-vm/cl:nil numbers stuff) *root*
      (setf borax-vm/cl:nil (make-instance 'borax-vm/cl:null))
      (setf numbers (borax-vm/cl:list -100 -3 0 1 2 3 4 5 43 343 8675309))
      (setf stuff (borax-vm/cl:list (borax-vm/cl:list 1 2 3)
                                    (borax-vm/cl:cons borax-vm/cl:nil borax-vm/cl:nil)
                                    (borax-vm/cl:list* 4 5 6 7)))
      *root*)))
