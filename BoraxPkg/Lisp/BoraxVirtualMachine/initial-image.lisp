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

(defvar *root* nil)

(defgeneric reify (object))

(define-modify-macro reify-place ()
  reify)

(defmethod reify ((object object))
  (dolist (loc (sub-objects object))
    (reify-place (dereference loc)))
  object)

(defmethod reify ((object null))
  (slot-value *root* 'borax-vm/cl:nil))

(defmethod reify ((object cons))
  (borax-vm/cl:cons (reify (car object))
                    (reify (cdr object))))

(defmethod reify ((object vector))
  (make-instance 'record-vector :data (map 'vector #'reify object)))

(defmethod reify ((object integer))
  ;; TODO: Arbitrary-precision integers
  object)

(defmethod reify ((object character))
  object)

(defun make-initial-image ()
  (let ((*root* (make-instance 'root)))
    (with-slots (borax-vm/cl:nil numbers stuff vector string) *root*
      (setf borax-vm/cl:nil (make-instance 'borax-vm/cl:null))
      (setf numbers '(-100 -3 0 1 2 3 4 5 43 343 8675309))
      (setf stuff '((1 2 3) (nil . nil) (4 5 6 . 7)))
      (setf vector #(#\A #\B #\C #\D))
      (setf string "Hellorld!")
      (reify *root*))))
