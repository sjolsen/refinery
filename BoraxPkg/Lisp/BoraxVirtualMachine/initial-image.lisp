(uiop:define-package :borax-virtual-machine/initial-image
  (:mix :closer-mop :uiop/common-lisp :borax-virtual-machine/common-lisp)
  (:use :cl-locatives
        :borax-virtual-machine/bytecode
        :borax-virtual-machine/image)
  (:export #:make-initial-image))

(in-package :borax-virtual-machine/initial-image)

(defclass borax-vm/cl:null ()
  ()
  (:metaclass record-class))

(defclass borax-vm/cl:package ()
  ((name :reader borax-vm/cl:package-name
         :initarg :name)
   (symbols :accessor package-symbols
            :initarg :symbols))
  (:metaclass record-class))

(defclass borax-vm/cl:symbol ()
  ((package :reader borax-vm/cl:symbol-package
            :initarg :package)
   (name :reader borax-vm/cl:symbol-name
         :initarg :name)
   (value :accessor borax-vm/cl:symbol-value)
   (class :accessor borax-vm/cl:find-class))
  (:metaclass record-class))

(defclass multiple-values ()
  ()
  (:metaclass record-class))

(defclass builder ()
  ((borax-vm/cl:nil :initform (make-instance 'borax-vm/cl:null))))

(defvar *builder* nil)

(define-symbol-macro borax-vm/cl:nil
    (slot-value *builder* 'borax-vm/cl:nil))

(defclass global-environment ()
  ((packages :accessor packages
             :initform borax-vm/cl:nil))
  (:metaclass record-class))

(defun borax-vm/cl:null (object)
  (eq object borax-vm/cl:nil))

(defun borax-vm/cl:string= (a b)
  (flet ((get-string (s)
           (etypecase s
             (string s)
             (borax-vm/cl:string (vector-data s)))))
    (string= (get-string a) (get-string b))))

(defun borax-vm/cl:find-package (name)
  (loop for l = (packages (root *image*))
          then (borax-vm/cl:cdr l)
        until (borax-vm/cl:null l)
        for package = (borax-vm/cl:car l)
        when (borax-vm/cl:string= (borax-vm/cl:package-name package) name)
          return package))

(defun ensure-package (package)
  (let ((name (cond
                ((eq (find-package :borax-vm/cl) package) "COMMON-LISP")
                (t (package-name package)))))
    (or (borax-vm/cl:find-package name)
        (borax-vm/cl:car
         (borax-vm/cl:push (make-instance 'borax-vm/cl:package
                                          :name (reify name)
                                          :symbols borax-vm/cl:nil)
                           (packages (root *image*)))))))

(defun borax-vm/cl:find-symbol (name package)
  (loop for l = (package-symbols package)
          then (borax-vm/cl:cdr l)
        until (borax-vm/cl:null l)
        for symbol = (borax-vm/cl:car l)
        when (borax-vm/cl:string= (borax-vm/cl:symbol-name symbol) name)
          return symbol))

(defun borax-vm/cl:intern (name package)
  (or (borax-vm/cl:find-symbol name package)
      (borax-vm/cl:car
       (borax-vm/cl:push (make-instance 'borax-vm/cl:symbol
                                        :name (reify name)
                                        :package package)
                         (package-symbols package)))))

;; TODO: If we ever end up with multiple instances of image generation code,
;; reify methods for CL classes will clash. This could be solved by adding a
;; root parameter for specialization.
(defmethod reify ((object null))
  borax-vm/cl:nil)

(defmethod reify ((object package))
  (ensure-package object))

(defmethod reify ((object symbol))
  (borax-vm/cl:intern (symbol-name object)
                      (reify (symbol-package object))))

(defmethod reify :after ((object borax-vm/cl:standard-class))
  (let* ((name (reify (borax-vm/cl:class-name object))))
    (setf (borax-vm/cl:find-class name) object)))

(defmacro borax-vm/cl:setq (&rest items)
  (loop for (symbol value) on items by #'cddr
        collecting `(setf (borax-vm/cl:symbol-value (reify ',symbol)) ,value)
          into forms
        finally (return `(progn ,@forms))))

(define-bytecode-function sum-list (&rest l)
  (declare (local l acc val))
    (bind (l))
    (move acc 0)
    (jump :if (not l) end)
  loop
    (call 'car-cdr (l))
    (bind (val l))
    (call '+ (acc val))
    (bind (acc))
    (jump :if l loop)
  end
    (return (acc)))

(defun make-initial-image ()
  (let ((*builder* (make-instance 'builder)))
    (setf (root *image*) (make-instance 'global-environment))
    (borax-vm/cl:setq
     borax-vm/cl:nil borax-vm/cl:nil
     numbers '(-100 -3 0 1 2 3 4 5 43 343 8675309)
     stuff '((1 2 3) (nil . nil) (4 5 6 . 7) :z)
     letters #(#\A #\B #\C #\D)
     hello "Hellorld!"
     sum-list (bytecode-function 'sum-list))
    (reify-image)))
