(uiop:define-package :borax-virtual-machine/bytecode
  (:mix :borax-parsing :uiop/common-lisp :borax-virtual-machine/common-lisp)
  (:use :borax-virtual-machine/image)
  (:export #:bytecode-function
           #:define-bytecode-function #:bytecode-disassemble
           #:bytecode #:bytecode-constants #:bytecode-locals #:bytecode-shared
           #:bytecode-closure #:bytecode-name #:bytecode-arglist #:bytecode-entry
           #:local #:shared #:closure
           #:call #:jump #:bind #:move
           #:class-typep))

(in-package :borax-virtual-machine/bytecode)

(defclass bytecode-function ()
  ((name :initarg :name
         :reader bytecode-name)
   (arglist :initarg :arglist
            :reader bytecode-arglist)
   (entry :initform 0
          :reader bytecode-entry)
   (code :initarg :code
         :reader bytecode)
   (locals :initarg :local
           :reader bytecode-locals)
   (shared :initarg :shared
           :reader bytecode-shared)
   (constants :initarg :constants
              :reader bytecode-constants))
  (:metaclass record-class))

(defstruct storage-block
  (count 0))

;; TODO: This should be called bytecode-function, and what is currently called
;; bytecode-function should be called borax-runtime:bytecode-function.
(defclass bytecode-parser ()
  ((name :initarg :name)
   (lambda-list :initarg :lambda-list)
   (constant-table :initform (make-hash-table))
   (symbol-table :initform (make-hash-table))
   (constants :initform (make-array 0 :adjustable t :fill-pointer 0))
   (locals :initform (make-storage-block))
   (shared :initform (make-array 0 :adjustable t))
   (closure :initform (make-array 0 :adjustable t))
   (code :initform (make-array 0 :element-type '(unsigned-byte 8)
                                 :adjustable t :fill-pointer 0))
   (label-table :initform (make-hash-table))
   (relocations :initform (make-array 0 :adjustable t :fill-pointer 0))
   (source :initform (make-array 0 :adjustable t :fill-pointer 0))))

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
  (if condition
      (destructuring-bind (test negatedp . operands) condition
        (values (ecase test
                  (identity    (if negatedp 2 1))
                  (eq          (if negatedp 4 3))
                  (class-typep (if negatedp 6 5)))
                operands))
      (values 0 nil)))

(defun emit-condition (offset bytespec condition)
  (with-slots (code) *parser-state*
    (multiple-value-bind (flag operands)
        (condition-code condition)
      (emit-field offset bytespec flag)
      (dolist (operand operands)
        (emit-location operand)))))

(defun emit-c-opcode (code condition values)
  (let ((offset (emit-byte (ash code 4))))
    (emit-condition offset (byte 1 3) condition)
    (emit-field offset (byte 3 0) (operand-count values))
    offset))

(defun emit-j-opcode (code condition)
  (let ((offset (emit-byte (ash code 4))))
    (emit-condition offset (byte 4 0) condition)
    offset))

(defun emit-b-opcode (code values)
  (let ((offset (emit-byte (ash code 4))))
    (emit-field offset (byte 4 0) (operand-count values))
    offset))

(defun flatten-storage-blocks (sbs)
  (loop with a = (make-array (length sbs) :element-type 'fixnum)
        for sb across sbs
        for i upfrom 0
        do (setf (aref a i) (storage-block-count sb))
        finally (return a)))

(defun record-instruction (offset instruction)
  (with-slots (source) *parser-state*
    (vector-push-extend (cons offset instruction) source)))

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
    (record-instruction offset (car (last (nonterminal-source))))
    (dolist (label labels)
      (declare-label label offset))))

(define-nonterminal condition ()
  (let ((nil :if)
        (result negatable-condition))
    result))

(define-nonterminal negatable-condition ()
  (nested (let ((nil  'not)
                (expr condition-expr))
            (destructuring-bind (test negatedp . locations) expr
              (list* test (not negatedp) locations))))
  condition-expr)

(define-nonterminal condition-expr ()
  (nested (let ((test (or 'eq 'class-typep))
                (l1   location)
                (l2   location))
            (list test nil l1 l2)))
  (let ((location location))
    (list 'identity nil location)))

(define-nonterminal values ()
  (let ((location location))
    (list :multiple-values location))
  (let ((locations (nested (* location))))
    (list* :values locations)))

(defun make-constant (value)
  (with-slots (constant-table constants) *parser-state*
    (or (gethash value constant-table)
        (prog1 (setf (gethash value constant-table)
                     (list 'constant nil (length constants)))
          (vector-push-extend value constants)))))

;; TODO: Numeric locations
(define-nonterminal location ()
  (let ((value (or boolean keyword)))
    (make-constant value))
  (let ((var symbol))
    (with-slots (symbol-table) *parser-state*
      (or (gethash var symbol-table)
          (error "Variable ~S not defined" var))))
  (let ((value atom))
    (make-constant value))
  (nested (let ((nil 'quote)
                (symbol symbol))
            (make-constant symbol))))

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

(defun bytecode-function (designator)
  (etypecase designator
    (bytecode-parser designator)
    (symbol (gethash designator *bytecode-functions*))))

(defun (setf bytecode-function) (value name)
  (assert (symbolp name))
  (assert (typep value 'bytecode-parser))
  (setf (gethash name *bytecode-functions*) value))

(define-condition bytecode-compile-error (error)
  ((name :initarg :name
         :reader bytecode-compiler-error-name)
   (original-error :initarg :original-error
                   :reader bytecode-compiler-error-original-error))
  (:report (lambda (c s)
             (format s "While byte-compiling ~A:~%~%~A"
                     (bytecode-compiler-error-name c)
                     (bytecode-compiler-error-original-error c)))))

(defun compile-bytecode-function (name lambda-list body)
  (let ((*parser-state* (make-instance 'bytecode-parser
                                       :name name
                                       :lambda-list lambda-list)))
    (handler-case
        (progn
          (parse-all 'bytecode-function (make-input body))
          (resolve-relocations))
       (error (c)
         (error 'bytecode-compile-error :name name :original-error c)))
     (setf (bytecode-function name) *parser-state*)))

(defmacro define-bytecode-function (name lambda-list &body body)
  `(compile-bytecode-function ',name ',lambda-list ',body))

(defmethod reify ((object bytecode-parser))
  (with-slots (name lambda-list code constants locals shared)
      object
    (make-instance 'bytecode-function
                   ;; TODO: use :type or something to do this at the CLOS level
                   :code (make-instance 'simple-vector-unsigned-byte-8
                                        :data (copy-seq code))
                   :constants (copy-seq constants)
                   :local (storage-block-count locals)
                   :shared (flatten-storage-blocks shared)
                   :name name
                   :arglist lambda-list)))

(defun print-table (lines stream)
  (let ((col0 0)
        (col1 0))
    (loop for (start bytes instruction) in lines
          do (setf col0 (max col0 (length start))
                   col1 (max col1 (length bytes))))
    (loop for (start bytes instruction) in lines
          do (format stream "  ~V<~A~>   ~V@<~A~>   ~A~%"
                     col0 start
                     col1 bytes
                     instruction))))

(defun bytecode-disassemble (designator &optional (stream *standard-output*))
  (with-slots (name source code)
      (bytecode-function designator)
    (format stream "Disassembly for bytecode function ~S:~%" name)
    (loop with length = (length source)
          for (start . instruction) across source
          for i from 1 upto length
          for end = (if (< i length)
                        (car (aref source i))
                        (length code))
          ;; TODO: Make this more efficient
          for bytes = (coerce (subseq code start end) 'list)
          collecting (list (format nil "~S" start)
                           (format nil "~{~2,'0X~^ ~}" bytes)
                           (format nil "~{~S~^ ~}" instruction))
            into lines
          finally (print-table lines stream))))
