(uiop:define-package :borax-virtual-machine/initial-image
  (:mix :uiop/common-lisp :borax-virtual-machine/image)
  (:shadow #:null #:nil #:list #:list*)
  (:export #:make-initial-image))

(in-package :borax-virtual-machine/initial-image)

(defclass null ()
  ()
  (:metaclass record-class))

(defclass root ()
  (nil
   numbers
   stuff)
  (:metaclass record-class))

(defvar *root* cl:nil)

(defun list (&rest values)
  (with-slots (nil) *root*
    (let ((result nil))
      (dolist (value (nreverse values))
        (borax-vm/image:push value result))
      result)))

(defun list* (&rest values)
  (with-slots (nil) *root*
    (destructuring-bind (result . rvalues)
        (nreverse values)
      (dolist (value rvalues)
        (borax-vm/image:push value result))
      result)))

(defun make-initial-image ()
  (let ((*root* (make-instance 'root)))
    (with-slots (nil numbers stuff) *root*
      (setf nil (make-instance 'null))
      (setf numbers (list -100 -3 0 1 2 3 4 5 43 343 8675309))
      (setf stuff (list (list 1 2 3)
                        (borax-vm/image:cons nil nil)
                        (list* 4 5 6 7)))
      *root*)))
