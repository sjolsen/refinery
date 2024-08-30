(uiop:define-package :borax-virtual-machine/initial-image
  (:mix :closer-mop :uiop/common-lisp :borax-virtual-machine/common-lisp)
  (:use :cl-locatives :borax-virtual-machine/image)
  (:export #:make-initial-image))

(in-package :borax-virtual-machine/initial-image)

(defclass borax-vm/cl:null ()
  ()
  (:metaclass record-class))

(defclass root ()
  (borax-vm/cl:nil
   numbers
   stuff
   vector
   string)
  (:metaclass record-class))

(defmethod reify ((object null))
  (slot-value (root *image*) 'borax-vm/cl:nil))

(defun make-initial-image ()
  (with-slots (borax-vm/cl:nil numbers stuff vector string)
      (setf (root *image*) (make-instance 'root))
    (setf borax-vm/cl:nil (make-instance 'borax-vm/cl:null))
    (setf numbers '(-100 -3 0 1 2 3 4 5 43 343 8675309))
    (setf stuff '((1 2 3) (nil . nil) (4 5 6 . 7)))
    (setf vector #(#\A #\B #\C #\D))
    (setf string "Hellorld!")
    (reify-image)))
