(uiop:define-package :borax-virtual-machine/bytecode
  (:mix :borax-parsing :uiop/common-lisp :borax-virtual-machine/common-lisp)
  (:use :borax-virtual-machine/image)
  (:export #:bytecode-function
           #:define-bytecode-function
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

(defstruct storage-block
  (count 0))

(defclass bytecode-parser ()
  ((constant-table :initform (make-hash-table))
   (symbol-table :initform (make-hash-table))
   (constants :initform (make-storage-block))
   (locals :initform (make-storage-block))
   (shared :initform (make-array 0 :adjustable t))
   (closure :initform (make-array 0 :adjustable t))
   (instructions :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defvar *parser-state* nil)

(defun get-storage-block (type &optional index)
  (let ((the-array
          (with-slots (locals shared closure)
              *parser-state*
            (ecase type
              (local (return-from get-storage-block locals))
              (shared shared)
              (closure closure)))))
    (unless (< index (length the-array))
      (adjust-array the-array (1+ index) :initial-element nil))
    (or (aref the-array index)
        (setf (aref the-array index) (make-storage-block)))))

(defun declare-variables (vars type &optional index)
  (with-slots (symbol-table) *parser-state*
    (with-slots (count) (get-storage-block type index)
      (dolist (var vars)
        (when (gethash var symbol-table)
          (error "Variable ~S already defined" var))
        (setf (gethash var symbol-table)
              (list type index count))
        (incf count)))))

(defun emit-instruction (&rest data)
  (list* 'instruction data))

(define-nonterminal bytecode-function ()
  (sequence (* (nested declaration))
            (+ labelled-instruction)))

(define-nonterminal declaration ()
  (sequence 'declare (* (nested declaration-specifier))))

;; TODO: Numeric declarations
(define-nonterminal declaration-specifier ()
  (let ((nil  'local)
        (vars (+ symbol)))
    (declare-variables vars 'local))
  (let ((type  (or 'shared 'closure))
        (index (satisfies integerp))
        (vars  (+ symbol)))
    (declare-variables vars type index)))

(define-nonterminal labelled-instruction ()
  (let ((labels      (* symbol))
        (instruction (nested instruction)))
    (with-slots (instructions) *parser-state*
      (vector-push-extend (cons labels instruction) instructions))))

(define-nonterminal condition ()
  (sequence :if condition-expr))

(define-nonterminal condition-expr ()
  (let ((location location))
    (list t location))
  (nested (sequence 'not location)))

(define-nonterminal values ()
  location
  (let ((locations (nested (* location))))
    ;; (optional values) will return nil when no value list is specified, which
    ;; is semantically distinct from an explicitly-specified empty list.
    (or locations :explicitly-empty)))

;; TODO: Numeric locations
(define-nonterminal location ()
  (let ((var symbol))
    (with-slots (symbol-table) *parser-state*
      (or (gethash var symbol-table)
          (error "Variable ~S not defined" var))))
  (let ((value constant))
    (with-slots (constant-table constants) *parser-state*
      (with-slots (count) constants
        (or (gethash value constant-table)
            (prog1 (setf (gethash value constant-table)
                         (list 'constant nil count))
              (incf count)))))))

(define-nonterminal constant ()
  number
  (let ((quote-form (nested (sequence 'quote symbol))))
    (cadr quote-form)))

(define-nonterminal instruction ()
  call-instruction
  jump-instruction
  return-instruction
  bind-instruction
  move-instruction)

(define-nonterminal call-instruction ()
  (let ((nil       'call)
        (tail      (optional :tail))
        (fast      (optional :fast))
        (condition (optional condition))
        (location  location)
        (values    (optional values)))
    (emit-instruction :op 'call
                      :tail (when tail t)
                      :fast (when fast t)
                      :condition condition
                      :location location
                      :values values)))

(define-nonterminal jump-instruction ()
  (let ((nil       'jump)
        (condition (optional condition))
        (index     symbol))
    (emit-instruction :op 'jump
                      :condition condition
                      :index index)))

(define-nonterminal return-instruction ()
  (let ((nil       'return)
        (condition (optional condition))
        (values    (optional values)))
    (emit-instruction :op 'return
                      :condition condition
                      :values values)))

(define-nonterminal bind-instruction ()
  (let ((nil    'bind)
        (values values))
    (emit-instruction :op 'bind
                      :values values)))

(define-nonterminal move-instruction ()
  (let ((nil         'move)
        (condition   (optional condition))
        (destination location)
        (source      location))
    (emit-instruction :op 'move
                      :condition condition
                      :destination destination
                      :source source)))

(defvar *bytecode-functions* (make-hash-table))

(defun bytecode-function (name)
  (assert (symbolp name))
  (gethash name *bytecode-functions*))

(defun (setf bytecode-function) (value name)
  (assert (symbolp name))
  ;; (assert (typep value 'bytecode-function))
  (setf (gethash name *bytecode-functions*) value))

(defmacro define-bytecode-function (name lambda-list &body body)
  ;; TODO: Parse lambda-lists
  (declare (ignore lambda-list))
  `(let ((*parser-state* (make-instance 'bytecode-parser)))
     (parse-all 'bytecode-function (make-input ',body))
     (setf (bytecode-function ',name) *parser-state*)))

(defun test()
  (define-bytecode-function test (&rest l)
    (declare (local l acc val))
      (bind (l))
      (move acc 0)
      (jump :if (not l) end)
    loop
      (call 'car-cdr (l))
      (bind (val l))
      (call '+ (acc val))
      (jump :if (not l) loop)
    end
      (return (acc))))
