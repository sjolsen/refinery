(uiop:define-package :borax-virtual-machine/initial-image
  (:mix :closer-mop :uiop/common-lisp :borax-virtual-machine/common-lisp)
  (:use :cl-locatives
        :borax-virtual-machine/bytecode
        :borax-virtual-machine/image)
  (:shadow #:class-precedence-list)
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

(defclass borax-vm/cl:character ()
  ()
  (:metaclass borax-vm/cl:class))

(defclass borax-vm/cl:fixnum ()
  ()
  (:metaclass borax-vm/cl:class))

(defclass borax-vm/cl:function ()
  ()
  (:metaclass borax-vm/cl:class))

(defclass borax-vm/cl:null (borax-vm/cl:list)
  ()
  (:metaclass borax-vm/cl:class))

(defclass built-in-function ()
  ()
  (:metaclass borax-vm/cl:class))

(defclass constant ()
  ()
  (:metaclass borax-vm/cl:class))

(defclass exit ()
  ()
  (:metaclass borax-vm/cl:class))

(defclass interpreter ()
  ()
  (:metaclass borax-vm/cl:class))

(defclass multiple-values ()
  ()
  (:metaclass borax-vm/cl:class))

(defclass pin ()
  ()
  (:metaclass borax-vm/cl:class))

(defclass task ()
  ()
  (:metaclass borax-vm/cl:class))

(defclass unbound ()
  ()
  (:metaclass borax-vm/cl:class))

(defclass weak-pointer ()
  ()
  (:metaclass borax-vm/cl:class))

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

(defclass class-not-found-error (borax-vm/cl:cell-error)
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
    (:borax-virtual-machine/image         . "BORAX-RUNTIME")
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

(define-bytecode-function borax-vm/cl:find (item list)
  (declare (local item list first match))
    (bind (item list))
  loop
    (jump :if (not list) empty)
    (call 'car-cdr (list))
    (bind (first list))
    (call 'borax-vm/cl:eq (item first))
    (bind (match))
    (return :if match (first))
    (jump loop)
  empty
    ;; TODO: Should we allocate another bit to condition codes for RETURN so we
    ;; can do (return :if (not list) (nil)) ?
    (return (nil)))

(define-bytecode-function borax-vm/cl:typep (object type)
  (declare (local object type class prec))
    (bind (object type))
    ;; TODO: Type specifiers and subclassing
    (call 'borax-vm/cl:find-class (type))
    (bind (type))
    (call 'borax-vm/cl:class-of (object))
    (bind (class))
    (call 'class-precedence-list (class))
    (bind (prec))
    (call :tail 'borax-vm/cl:find (type prec)))

(define-bytecode-function format-list (object)
  (declare (local object match item rest))
    (call 'print-character (#\())
    (call 'car-cdr (object))
    (bind (item rest))
    (call 'format-recursive (item))
  loop
    (jump :if (not rest) loop-end)
    (call 'borax-vm/cl:typep (object 'cons))
    (bind (match))
    (jump :if (not match) loop-rest)
    (call 'print-character (#\Space))
    (call 'car-cdr (object))
    (bind (item rest))
    (call 'format-recursive (item))
    (jump loop)
  loop-rest
    (call 'print-string (" . "))
    (call 'format-recursive (rest))
  loop-end
    (call 'format-character (#\)))
    (return (object)))

(define-bytecode-function format-recursive (object)
  (declare (local object match rest))
    (bind (object))
    ; nil
    (jump :if object not-nil)
    (call 'print-string ("NIL"))
    (return (object))
  not-nil
    ; fixnum
    (call 'borax-vm/cl:typep (object 'borax-vm/cl:fixnum))
    (bind (match))
    (call :tail :if match 'print-integer (object))
    ; character
    (call 'borax-vm/cl:typep (object 'borax-vm/cl:character))
    (bind (match))
    (jump :if (not match) not-char)
    (call 'print-string ("#\\"))
    ;; TODO: non-printable characters
    (call :tail 'print-character (object))
  not-char
    ; cons
    (call 'borax-vm/cl:typep (object 'borax-vm/cl:cons))
    (bind (match))
    (call :tail :if match 'print-list (object))
    ; string
    (call 'borax-vm/cl:typep (object 'borax-vm/cl:string))
    (bind (match))
    (jump :if (not match) not-string)
    (call 'print-character (#\"))
    (call 'print-string (object))
    (call 'print-character (#\"))
  not-string
    ; simple-vector-unsigned-byte-8
    (call 'borax-vm/cl:typep (object 'simple-vector-unsigned-byte-8))
    (bind (match))
    (call :tail :if match 'print-byte-vector (object))
    ; other word-record
    (call 'borax-vm/cl:typep (object 'word-record-object))
    (bind (match))
    (jump :if (not match) not-word-record)
    (call 'print-string ("<WORD-RECORD>"))
    (return (object))
  not-word-record
    ; symbol
    (call 'borax-vm/cl:typep (object 'borax-vm/cl:symbol))
    (bind (match))
    (call :tail :if match 'print-symbol (object))
    ; simple-vector
    (call 'borax-vm/cl:typep (object 'borax-vm/cl:simple-vector))
    (bind (match))
    (call :tail :if match 'print-vector (object))
    ; standard-class
    (call 'borax-vm/cl:typep (object 'borax-vm/cl:standard-class))
    (bind (match))
    (call :tail :if match 'print-standard-class (object))
    ; other object-record
    (call 'borax-vm/cl:typep (object 'record-object))
    (bind (match))
    (call :tail :if match 'print-object-record (object))
    ; weak-pointer
    (call 'borax-vm/cl:typep (object 'weak-pointer))
    (bind (match))
    (jump :if (not match) not-weak-pointer)
    (call 'print-string ("<WEAK-POINTER>"))
    (return (object))
  not-weak-pointer
    ; pin
    (call 'borax-vm/cl:typep (object 'pin))
    (bind (match))
    (jump :if (not match) not-pin)
    (call 'print-string ("<PIN>"))
    (return (object))
  not-pin
    (call 'error ('type-error ':datum object ':expected-type 't)))

(define-bytecode-function print-labelled (symbol)
  (declare (local symbol value))
    (bind (symbol))
    (call 'symbol-name (symbol))
    (call 'write-string)
    (call 'write-string (" = "))
    (call 'symbol-value (symbol))
    (call 'write-string)
    (call 'write-character (#\Newline)))

(define-bytecode-function print-sum-list (symbol)
  (declare (local symbol value))
    (bind (symbol))
    (call 'write-string ("(SUM-LIST "))
    (call 'symbol-name (symbol))
    (call 'write-string)
    (call 'write-string (") = "))
    (call 'symbol-value (symbol))
    (call 'sum-list)
    (call 'print-recursive)
    (call 'write-character (#\Newline)))

(define-bytecode-function demo ()
  (call 'print-labelled ('numbers))
  (call 'print-labelled ('stuff))
  (call 'print-labelled ('letters))
  (call 'print-labelled ('hello))
  (call 'print-labelled ('sum-list))
  (call 'print-sum-list ('sum-list)))

(defun ensure-bytecode-function (name)
  (setf (borax-vm/cl:symbol-function (reify name))
        (bytecode-function name)))

(defun make-initial-image ()
  (setf (root *image*) (make-instance 'global-environment))
  ;; Standard classes
  (ensure-find-class 'borax-vm/cl:character)
  (ensure-find-class 'borax-vm/cl:cons)
  (ensure-find-class 'borax-vm/cl:fixnum)
  (ensure-find-class 'borax-vm/cl:function)
  (ensure-find-class 'borax-vm/cl:list)
  (ensure-find-class 'borax-vm/cl:null)
  (ensure-find-class 'borax-vm/cl:package)
  (ensure-find-class 'borax-vm/cl:simple-error)
  (ensure-find-class 'borax-vm/cl:simple-vector)
  (ensure-find-class 'borax-vm/cl:standard-class)
  (ensure-find-class 'borax-vm/cl:t)
  (ensure-find-class 'borax-vm/cl:type-error)
  (ensure-find-class 'borax-vm/cl:undefined-function)
  ;; Built-in classes
  (ensure-find-class 'built-in-function)
  (ensure-find-class 'class-not-found-error)
  (ensure-find-class 'constant)
  (ensure-find-class 'exit)
  (ensure-find-class 'heap-exhausted)
  (ensure-find-class 'interpreter)
  (ensure-find-class 'location-error)
  (ensure-find-class 'multiple-values)
  (ensure-find-class 'pin)
  (ensure-find-class 'record-object)
  (ensure-find-class 'simple-program-error)
  (ensure-find-class 'simple-vector-unsigned-byte-8)
  (ensure-find-class 'stack-exhausted)
  (ensure-find-class 'task)
  (ensure-find-class 'unbound)
  (ensure-find-class 'weak-pointer)
  (ensure-find-class 'word-record-object)
  ;; Built-in functions
  (ensure-bytecode-function 'print-labelled)
  (ensure-bytecode-function 'format-recursive)
  (ensure-bytecode-function 'borax-vm/cl:find)
  (ensure-bytecode-function 'borax-vm/cl:typep)
  ;; Demo content
  (borax-vm/cl:setq
   borax-vm/cl:nil borax-vm/cl:nil
   numbers '(-100 -3 0 1 2 3 4 5 43 343 8675309)
   stuff '((1 2 3) (nil . nil) (4 5 6 . 7) :z)
   letters #(#\A #\B #\C #\D)
   hello "Hellorld!"
   sum-list (bytecode-function 'sum-list))
  (ensure-bytecode-function 'demo)
  (reify-image))
