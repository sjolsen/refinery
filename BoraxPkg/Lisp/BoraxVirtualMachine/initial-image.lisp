(uiop:define-package :borax-virtual-machine/initial-image
  (:mix :closer-mop :uiop/common-lisp :borax-virtual-machine/common-lisp)
  (:use :cl-locatives :borax-virtual-machine/image)
  (:export #:make-initial-image))

(in-package :borax-virtual-machine/initial-image)

(defclass borax-vm/cl:null ()
  ()
  (:metaclass record-class))

(defclass borax-vm/cl:package ()
  ((name :reader borax-vm/cl:package-name
         :initarg :name))
  (:metaclass record-class))

(defclass borax-vm/cl:symbol ()
  ((package :reader borax-vm/cl:symbol-package
            :initarg :package)
   (name :reader borax-vm/cl:symbol-name
         :initarg :name))
  (:metaclass record-class))

(defclass globals ()
  ((borax-vm/cl:nil :initform (make-instance 'borax-vm/cl:null)))
  (:metaclass record-class))

(defclass classes ()
  (borax-vm/cl:standard-class
   borax-vm/cl:package
   borax-vm/cl:symbol
   borax-vm/cl:simple-vector
   borax-vm/cl:string)
  (:metaclass record-class))

(defmethod initialize-instance :after ((instance classes) &key)
  (do-record-slots (place slot) instance
    (setf place (find-class (slot-definition-name slot)))))

(defclass root ()
  ((globals :accessor globals
            :initform (make-instance 'globals))
   (classes :accessor classes
            :initform (make-instance 'classes))
   numbers
   stuff
   vector
   hello)
  (:metaclass record-class))

;; TODO: If we ever end up with multiple instances of image generation code,
;; reify methods for CL classes will clash. This could be solved by adding a
;; root parameter for specialization.
(defmethod reify ((object null))
  (slot-value (globals (root *image*)) 'borax-vm/cl:nil))

(defmethod reify ((object package))
  (make-instance 'borax-vm/cl:package :name (package-name object)))

(defmethod reify ((object symbol))
  (make-instance 'borax-vm/cl:symbol
                 :package (symbol-package object)
                 :name (symbol-name object)))

(defun make-initial-image ()
  (with-slots (numbers stuff vector hello)
      (setf (root *image*) (make-instance 'root))
    (setf numbers '(-100 -3 0 1 2 3 4 5 43 343 8675309))
    (setf stuff '((1 2 3) (nil . nil) (4 5 6 . 7)))
    (setf vector #(#\A #\B #\C #\D))
    (setf hello "Hellorld!")
    (reify-image)))
