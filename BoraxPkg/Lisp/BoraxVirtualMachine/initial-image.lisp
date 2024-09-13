(uiop:define-package :borax-virtual-machine/initial-image
  (:mix :closer-mop :uiop/common-lisp :borax-virtual-machine/common-lisp)
  (:use :cl-locatives
        :borax-virtual-machine/bytecode
        :borax-virtual-machine/image)
  (:export #:make-initial-image))

(in-package :borax-virtual-machine/initial-image)

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
   (function :accessor borax-vm/cl:symbol-function)
   (class :accessor borax-vm/cl:find-class))
  (:metaclass record-class))

(defclass borax-vm/cl:function ()
  ()
  (:metaclass record-class))

(defclass borax-vm/cl:condition ()
  ()
  (:metaclass record-class))

(defclass borax-vm/cl:simple-condition (borax-vm/cl:condition)
  (format-control
   format-arguments)
  (:metaclass record-class))

(defclass borax-vm/cl:serious-condition (borax-vm/cl:condition)
  ()
  (:metaclass record-class))

(defclass borax-vm/cl:error (borax-vm/cl:serious-condition)
  ()
  (:metaclass record-class))

(defclass borax-vm/cl:simple-error (borax-vm/cl:simple-condition
                                    borax-vm/cl:error)
  ()
  (:metaclass record-class))

(defclass borax-vm/cl:program-error (borax-vm/cl:error)
  ()
  (:metaclass record-class))

(defclass simple-program-error (borax-vm/cl:simple-error
                                borax-vm/cl:program-error)
  ()
  (:metaclass record-class))

(defclass borax-vm/cl:type-error (borax-vm/cl:error)
  (datum
   expected-type)
  (:metaclass record-class))

(defclass borax-vm/cl:storage-condition (borax-vm/cl:serious-condition)
  ()
  (:metaclass record-class))

(defclass heap-exhausted (borax-vm/cl:storage-condition)
  ()
  (:metaclass record-class))

(defclass stack-exhausted (borax-vm/cl:storage-condition)
  ()
  (:metaclass record-class))

(defclass borax-vm/cl:cell-error (borax-vm/cl:error)
  ()
  (:metaclass record-class))

(defclass borax-vm/cl:undefined-function (borax-vm/cl:cell-error)
  ()
  (:metaclass record-class))

(defclass location-error (borax-vm/cl:cell-error)
  ()
  (:metaclass record-class))

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

(defun ensure-package (name)
  (or (borax-vm/cl:find-package name)
      (borax-vm/cl:car
       (borax-vm/cl:push (make-instance 'borax-vm/cl:package
                                        :name (reify name)
                                        :symbols borax-vm/cl:nil)
                         (packages (root *image*))))))

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

(defun ensure-find-class (class)
  ;; TODO: This typecase is necessary to prevent infinite recursion of the
  ;; :after method on reify for borax-vm/cl:standard-class, which should
  ;; probably be implemented a different way.
  ;;
  ;; TODO: Also borax-vm/cl:class should be named something else since it's a
  ;; host object, not a reified target object.
  (let* ((class (etypecase class
                  (symbol (reify (find-class class)))
                  (borax-vm/cl:class (reify class))
                  (borax-vm/cl:standard-class class)))
         (name (reify (borax-vm/cl:class-name class))))
    (unless (slot-boundp name 'class)
      (setf (borax-vm/cl:find-class name) class))
    class))

(defparameter *package-rename-alist*
  '((:borax-vm/cl . "COMMON-LISP")
    ;; TODO: Make this a real package (but not the real borax-runtime package
    ;; because that will cause a conflict when we self-host)
    (:borax-virtual-machine/initial-image . "BORAX-RUNTIME")
    (:borax-virtual-machine/bytecode      . "BORAX-RUNTIME")))

;; TODO: If we ever end up with multiple instances of image generation code,
;; reify methods for CL classes will clash. This could be solved by adding a
;; root parameter for specialization.
(defmethod reify ((object package))
  (let ((name (loop for (designator . name) in *package-rename-alist*
                    when (eq object (find-package designator))
                      do (return name)
                    finally (return (package-name object)))))
    (ensure-package name)))

(defmethod reify ((object symbol))
  (borax-vm/cl:intern (symbol-name object)
                      (reify (symbol-package object))))

(defmethod reify :after ((object borax-vm/cl:standard-class))
  (ensure-find-class object))

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
  (setf (root *image*) (make-instance 'global-environment))
  (ensure-find-class 'borax-vm/cl:function)
  (ensure-find-class 'borax-vm/cl:simple-error)
  (ensure-find-class 'borax-vm/cl:type-error)
  (ensure-find-class 'borax-vm/cl:undefined-function)
  (ensure-find-class 'simple-program-error)
  (ensure-find-class 'heap-exhausted)
  (ensure-find-class 'stack-exhausted)
  (ensure-find-class 'location-error)
  (let ((keyword (ensure-package "KEYWORD")))
    (borax-vm/cl:intern "CONSTANT" keyword)
    (borax-vm/cl:intern "LOCAL" keyword)
    (borax-vm/cl:intern "SHARED" keyword))
  (borax-vm/cl:setq
   borax-vm/cl:nil borax-vm/cl:nil
   numbers '(-100 -3 0 1 2 3 4 5 43 343 8675309)
   stuff '((1 2 3) (nil . nil) (4 5 6 . 7) :z)
   letters #(#\A #\B #\C #\D)
   hello "Hellorld!"
   sum-list (bytecode-function 'sum-list))
  (setf (borax-vm/cl:symbol-function (reify 'sum-list))
        (bytecode-function 'sum-list))
  (reify-image))
