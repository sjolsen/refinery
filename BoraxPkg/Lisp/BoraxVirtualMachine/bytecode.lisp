(uiop:define-package :borax-virtual-machine/bytecode
  (:mix :uiop/common-lisp :borax-virtual-machine/common-lisp)
  (:use :borax-virtual-machine/image)
  (:export #:bytecode-function
           #:compile-bytecode
           #:call #:jump #:bind #:move))

(in-package :borax-virtual-machine/bytecode)

(defclass bytecode-function ()
  ((name :initarg :name)
   (arglist :initarg :arglist)
   (locals :accessor local)
   (shared :accessor shared)
   (constants :accessor constants)
   (entry :initform 0)
   (code :accessor code))
  (:metaclass record-class))

(defclass instruction ()
  (label :initarg :label))

(defun compile-bytecode (name arglist instructions)
  (let ((locals 0)
        (shared (make-array 0 :element-type 'fixnum
                              :adjustable t
                              :fill-pointer 0))
        (constants (make-array 0 :element-type t
                                 :adjustable t
                                 :fill-pointer 0))
        (code (make-array 0 :element-type t ; '(unsigned-byte 8)
                            :adjustable t
                            :fill-pointer 0))
        (labels nil))
    (dolist (inst instructions)
      (parse-instruction))
    (let ((f (make-instance 'bytecode-function
                            :name name
                            :arglist arglist)))
      (setf (locals f) locals)
      (when (/= 0 (length shared))
        (setf (shared f) shared))
      (when (/= 0 (length constants))
        (setf (constants f) constants))
      (setf (code f) code)
      f)))

(define-condition parse-error (error)
  ((expected :initarg :expected)
   (actual :initarg :actual)
   (item-number :initarg :item-number)))

(defun assemble-bytecode-function (name arglist body)
  (let (;; Number of local slots
        (local 0)
        ;; Number of shared slots by block index
        (shared (make-array :adjustable t :fill-pointer 0))
        ;; Number of closure slots by block index
        (closure (make-array :adjustable t :fill-pointer 0))
        ;; alist from variable names to slots
        (vars nil)
        ;; Labels for the next instruction
        (labels nil)
        ;; ...
        (code (make-array :adjustable t :fill-pointer 0)))
    (loop for item in body
          for item-number upfrom 0
          do (typecase item
               (symbol (push item labels))
               (cons
                (ecase (car item)
                  (declare
                   (dolist (clause (cdr item))
                     (parse-declaration clause item-number)))
                  (t
                   (parse-instruction item labels)
                   (setf labels nil))))
               (t (error 'parse-error
                         :expected "declaration, label, or instruction"
                         :actual item
                         :item-number item-number))))))

(defmacro define-bytecode-function (name arglist &body body)
  ;; TODO
  (break))

(define-bytecode-function test (&rest l)
  (declare (local acc val))
    (bind (l))
    (move acc 0)
    (jump :if (:not l) end)
  loop
    (call car-cdr (l))
    (bind (val l))
    (call + (acc val))
    (jump :if (:not l) loop)
  end
    (return (acc)))
