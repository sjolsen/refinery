(uiop:define-package :borax-virtual-machine/initial-image
  (:mix :closer-mop :uiop/common-lisp :borax-virtual-machine/common-lisp)
  (:use :cl-locatives
        :borax-virtual-machine/bytecode
        :borax-virtual-machine/image)
  (:shadow #:write-character #:write-string)
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

(defmacro borax-vm/cl:setq (&rest items)
  (loop for (symbol value) on items by #'cddr
        collecting `(setf (borax-vm/cl:symbol-value (reify ',symbol)) ,value)
          into forms
        finally (return `(progn ,@forms))))

(define-bytecode-function sum-list (&rest l)
  (declare (local l acc val))
    (bind (l))
    (move acc 0)
    (return :if (not l) (acc))
  loop
    (call 'car-cdr (l))
    (bind (val l))
    (call '+ (acc val))
    (bind (acc))
    (jump :if l loop)
  end
    (return (acc)))

(define-bytecode-function borax-vm/cl:find (item list)
  (declare (local item list first))
    (bind (item list))
  loop
    (return :if (not list) (nil))
    (call 'car-cdr (list))
    (bind (first list))
    (jump :if (not (eq item first)) loop)
    (return (first)))

(define-bytecode-function borax-vm/cl:typep (object type)
  (declare (local object type prec))
    (bind (object type))
    (return :if (class-typep object type) (t))
    ;; TODO: Type specifiers and subclassing
    (return (nil)))

(define-bytecode-function borax-vm/cl:check-type (object type)
  (declare (local object type match))
    (bind (object type))
    (call 'borax-vm/cl:typep (object type))
    (bind (match))
    (return :if match (nil))
    (call 'borax-vm/cl:error ('borax-vm/cl:type-error
                              :datum object :expected-type type)))

(defmacro define-bytecode-accessor (name class slot)
  (let* ((class (find-class class))
         (slot (find slot (class-slots class) :key #'slot-definition-name))
         (index (record-slot-location slot))
         ;; TODO: Figure out how to support GENSYM
         (var 'object))
    `(define-bytecode-function ,name (,var)
       (declare (local ,var))
       (bind (,var))
       (call 'borax-vm/cl:check-type (,var ,class))
       (call :tail 'record-slot (,var ,index)))))

;; TODO: Generate these from the class definition
(define-bytecode-accessor borax-vm/cl:class-name
  borax-vm/cl:standard-class borax-virtual-machine/image::name)

(define-bytecode-accessor borax-vm/cl:package-name
  borax-vm/cl:package name)

(define-bytecode-accessor borax-vm/cl:symbol-package
  borax-vm/cl:symbol package)

(define-bytecode-accessor borax-vm/cl:symbol-name
  borax-vm/cl:symbol name)

(define-bytecode-accessor borax-vm/cl:symbol-value
  borax-vm/cl:symbol value)

(define-bytecode-accessor borax-vm/cl:symbol-function
  borax-vm/cl:symbol function)

(define-bytecode-accessor borax-vm/cl:find-class
  borax-vm/cl:symbol class)

(define-bytecode-function print-list (object)
  (declare (local object item rest))
    (bind (object))
    (call 'write-character (#\())
    (call 'car-cdr (object))
    (bind (item rest))
    (call 'print-recursive (item))
  loop
    (jump :if (not rest) loop-end)
    (jump :if (not (class-typep rest 'cons)) loop-rest)
    (call 'write-character (#\Space))
    (call 'car-cdr (rest))
    (bind (item rest))
    (call 'print-recursive (item))
    (jump loop)
  loop-rest
    (call 'write-string (" . "))
    (call 'print-recursive (rest))
  loop-end
    (call 'write-character (#\)))
    (return (object)))

(define-bytecode-function print-byte-vector (object)
  (declare (local object length i item))
    (bind (object))
    (call 'byte-vector-length)
    (bind (length))
    (call 'write-string ("<BYTE-VECTOR"))
    (move i 0)
    (jump loop-test)
  loop
    (call 'write-character (#\Space))
    (call 'byte-vector-ref (object i))
    (call 'print-byte)
    (call '+ (i 1))
    (bind (i))
  loop-test
    (jump :if (not (eq i length)) loop)
    (call 'write-character (#\>))
    (return (object)))

(define-bytecode-function print-symbol (object)
  (declare (local object package other-package))
    (bind (object))
    (call 'symbol-package)
    (bind (package))
    (call 'borax-vm/cl:find-package ("COMMON-LISP"))
    (bind (other-package))
    (jump :if (eq package other-package) name)
    (call 'borax-vm/cl:find-package ("KEYWORD"))
    (bind (other-package))
    (jump :if (eq package other-package) colon)
    (call 'borax-vm/cl:package-name (package))
    (call 'write-string)
  colon
    (call 'write-character (#\Colon))
  name
    (call 'symbol-name (object))
    (call 'write-string)
    (return (object)))

(define-bytecode-function print-object-record (object)
  (declare (local object class-name length i))
    (bind (object))
    (call 'borax-vm/cl:class-of)
    (call 'borax-vm/cl:class-name)
    (bind (class-name))
    (call 'write-character (#\<))
    (call 'print-symbol (class-name))
    (call 'record-length (object))
    (bind (length))
    (move i 0)
    (jump loop-test)
  loop
    (call 'write-character (#\Space))
    (call 'print-fixnum (i))
    (call 'write-character (#\=))
    (call 'record-slot (object i))
    (call 'print-recursive)
    (call '+ (i 1))
    (bind (i))
  loop-test
    (jump :if (not (eq i length)) loop)
    (call 'write-character (#\>))
    (return (object)))

(define-bytecode-function print-recursive (object)
  (declare (local object rest))
    (bind (object))
    ; nil
    (jump :if object not-nil)
    (call 'write-string ("NIL"))
    (return (object))
  not-nil
    ; fixnum
    (call :tail :if (class-typep object 'borax-vm/cl:fixnum)
          'print-fixnum (object))
    ; character
    (jump :if (not (class-typep object 'borax-vm/cl:character)) not-char)
    (call 'write-string ("#\\"))
    ;; TODO: non-printable characters
    (call 'write-character (object))
    (return (object))
  not-char
    ; cons
    (call :tail :if (class-typep object 'borax-vm/cl:cons)
          'print-list (object))
    ; string
    (jump :if (not (class-typep object 'borax-vm/cl:string)) not-string)
    (call 'write-character (#\"))
    (call 'write-string (object))
    (call 'write-character (#\"))
    (return (object))
  not-string
    ; simple-vector-unsigned-byte-8
    (call :tail :if (class-typep object 'simple-vector-unsigned-byte-8)
          'print-byte-vector (object))
    ; other word-record
    (jump :if (not (class-typep object 'word-record-object)) not-word-record)
    (call 'write-string ("<WORD-RECORD>"))
    (return (object))
  not-word-record
    ; symbol
    (call :tail :if (class-typep object 'borax-vm/cl:symbol)
          'print-symbol (object))
    ; simple-vector
    (call :tail :if (class-typep object 'borax-vm/cl:simple-vector)
          'print-simple-vector (object))
    ; standard-class
    (call :tail :if (class-typep object 'borax-vm/cl:standard-class)
          'print-standard-class (object))
    ; other object-record
    (call :tail :if (class-typep object 'record-object)
          'print-object-record (object))
    ; weak-pointer
    (jump :if (not (class-typep object 'weak-pointer)) not-weak-pointer)
    (call 'write-string ("<WEAK-POINTER>"))
    (return (object))
  not-weak-pointer
    ; pin
    (jump :if (not (class-typep object 'pin)) not-pin)
    (call 'write-string ("<PIN>"))
    (return (object))
  not-pin
    (call 'error ('type-error :datum object :expected-type 't)))

(define-bytecode-function print-labelled (symbol)
  (declare (local symbol value))
    (bind (symbol))
    (call 'symbol-name (symbol))
    (call 'write-string)
    (call 'write-string (" = "))
    (call 'symbol-value (symbol))
    (call 'print-recursive)
    (call 'write-character (#\Newline))
    (return))

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
    (call 'write-character (#\Newline))
    (return))

(define-bytecode-function demo ()
  (call 'print-labelled ('numbers))
  (call 'print-labelled ('stuff))
  (call 'print-labelled ('letters))
  (call 'print-labelled ('hello))
  (call 'print-labelled ('sum-list))
  (call 'print-sum-list ('numbers))
  (return))

(defun ensure-bytecode-function (name)
  (setf (borax-vm/cl:symbol-function (reify name))
        (bytecode-function name)))

(defun make-initial-image ()
  (setf (root *image*) (make-instance 'global-environment))
  ;; TODO: Principled namespace management
  (loop for class across +classes+
        do (ensure-find-class class))
  (loop for name being each hash-key in *bytecode-functions*
        do (ensure-bytecode-function name))
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
