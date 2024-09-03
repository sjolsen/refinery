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
   (code :initform (make-array 0 :element-type '(unsigned-byte 8)
                                 :adjustable t :fill-pointer 0))
   (label-table :initform (make-hash-table))
   (relocations :initform (make-array 0 :adjustable t :fill-pointer 0))))

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

(defun declare-label (label offset)
  (assert (typep offset '(unsigned-byte 16)))
  (with-slots (label-table) *parser-state*
    (when (gethash label label-table)
      (error "Label ~S already defined" label))
    (setf (gethash label label-table) offset)))

(defun emit-byte (value)
  (assert (typep value '(unsigned-byte 8)))
  (with-slots (code) *parser-state*
    (vector-push-extend value code)))

(defun emit-index (value &key overwrite)
  (assert (typep value '(unsigned-byte 16)))
  (with-slots (code) *parser-state*
    (let ((low-byte (ldb (byte 8 0) value))
          (high-byte (ldb (byte 8 8) value)))
      (if overwrite
          (prog1 (setf (aref code overwrite) low-byte)
            (setf (aref code (1+ overwrite)) high-byte))
          (prog1 (vector-push-extend low-byte code)
            (vector-push-extend high-byte code))))))

(defun emit-field (offset bytespec value)
  (with-slots (code) *parser-state*
    (let ((limit (1- (ash 1 (byte-size bytespec)))))
      (setf (ldb bytespec (aref code offset))
            (min value limit))
      (when (>= value limit)
        (emit-byte value)))))

(defun emit-location (location)
  (destructuring-bind (type block-index var-index) location
    (let* ((tag (ecase type
                  (constant 0)
                  (local    1)
                  (shared   2)
                  (closure  3)))
           (offset (emit-byte (ash tag 6))))
      (ecase type
        ((constant local)
         (assert (null block-index))
         (emit-field offset (byte 6 0) var-index))
        ((shared closure)
         (assert block-index)
         (emit-field offset (byte 3 3) block-index)
         (emit-field offset (byte 3 0) var-index))))))

(defun emit-values (values)
  (dolist (value (cdr values))
    (emit-location value)))

(defun emit-jump-target (label)
  (with-slots (relocations) *parser-state*
    (let ((offset (emit-index 0)))
      (vector-push-extend (cons label offset)
                          relocations))))

(defun resolve-relocations ()
  (with-slots (label-table relocations) *parser-state*
    (loop for (label . offset) across relocations
          for target = (gethash label label-table)
          when (null target)
            do (error "Jump to undefined label ~S" label)
          do (emit-index target :overwrite offset))))

(defun operand-count (values)
  (ecase (car values)
    ((nil)            0)
    (:multiple-values 1)
    (:values          (+ 2 (length (cdr values))))))

(defun condition-code (condition)
  (case (car condition)
    ((nil)    (values 0 nil))
    (identity (values 1 (cdr condition)))
    (not      (values 2 (cdr condition)))))

(defun emit-condition (offset bytespec condition)
  (with-slots (code) *parser-state*
    (multiple-value-bind (flag operands)
        (condition-code condition)
      (when (> (integer-length flag) (byte-size bytespec))
        (error "Condition ~S cannot be encoded" condition))
      (setf (ldb bytespec (aref code offset)) flag)
      (dolist (operand operands)
        (emit-location operand)))))

(defun emit-c-opcode (code condition values)
  (let ((offset (emit-byte (ash code 4))))
    (emit-field offset (byte 3 0) (operand-count values))
    (emit-condition offset (byte 1 3) condition)
    offset))

(defun emit-j-opcode (code condition)
  (let ((offset (emit-byte (ash code 4))))
    (emit-condition offset (byte 4 0) condition)
    offset))

(defun emit-b-opcode (code values)
  (let ((offset (emit-byte (ash code 4))))
    (emit-field offset (byte 4 0) (operand-count values))
    offset))

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
  (let ((labels (* symbol))
        (offset (nested instruction)))
    (dolist (label labels)
      (declare-label label offset))))

(define-nonterminal condition ()
  (let ((nil :if)
        (result condition-expr))
    result))

(define-nonterminal condition-expr ()
  (let ((location location))
    (list 'identity location))
  (nested (sequence 'not location)))

(define-nonterminal values ()
  (let ((location location))
    (list :multiple-values location))
  (let ((locations (nested (* location))))
    (list* :values locations)))

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
    (prog1
        (let ((code 0))
          (when fast
            (setf code (logior code 1)))
          (when tail
            (setf code (logior code 2)))
          (emit-c-opcode code condition values))
      (emit-location location)
      (emit-values values))))

(define-nonterminal jump-instruction ()
  (let ((nil       'jump)
        (condition (optional condition))
        (target    symbol))
    (prog1 (emit-j-opcode 4 condition)
      (emit-jump-target target))))

(define-nonterminal return-instruction ()
  (let ((nil       'return)
        (condition (optional condition))
        (values    (optional values)))
    (prog1 (emit-c-opcode 5 condition values)
      (emit-values values))))

(define-nonterminal bind-instruction ()
  (let ((nil    'bind)
        (values values))
    (prog1 (emit-b-opcode 13 values)
      (emit-values values))))

(define-nonterminal move-instruction ()
  (let ((nil         'move)
        (condition   (optional condition))
        (destination location)
        (source      location))
    (prog1 (emit-j-opcode 15 condition)
      (emit-location destination)
      (emit-location source))))

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
     (resolve-relocations)
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
