(uiop:define-package :borax-parsing/parsing
  (:use :uiop/common-lisp)
  (:shadow #:parse-error #:satisfies #:sequence)
  (:export #:input #:make-input
           #:parse-error
           #:parser #:description
           #:parse #:parse-all #:try-parse
           #:nonterminal-source
           #:anything-parser #:*anything*
           #:end-of-input-parser #:*end-of-input*
           #:satisfies-parser #:satisfies
           #:literal-parser #:literal
           #:sequence-parser #:sequence
           #:alternative-parser #:alternative
           #:repeat-parser #:repeat
           #:at-least-once-parser #:at-least-once
           #:optional-parser #:optional #:?
           #:nested-parser #:nested
           #:nonterminal
           #:define-nonterminal))

(in-package :borax-parsing/parsing)

(defstruct (input (:constructor %make-input))
  source
  rest
  (position 0))

(defun make-input (s)
  (%make-input :source s :rest s))

(defun input-location (input)
  (list (input-position input)
        (input-source input)))

(defgeneric split-seq (s))

(defmethod split-seq ((s cons))
  (values (car s) (cdr s) t))

(defmethod split-seq ((s null))
  (values nil nil nil))

(defun end-of-input-p (input)
  (multiple-value-bind (first rest valid)
      (split-seq (input-rest input))
    (declare (ignore first rest))
    (not valid)))

(defun peek (input)
  (multiple-value-bind (first rest valid)
      (split-seq (input-rest input))
    (declare (ignore rest))
    (values first valid)))

(defun advance (input)
  (multiple-value-bind (first rest valid)
      (split-seq (input-rest input))
    (declare (ignore first))
    (when valid
      (setf (input-rest input) rest)
      (incf (input-position input)))
    (values)))

(defclass parser ()
  ((name :initarg :name)
   (action :initarg :action)))

(defgeneric description (parser))

(defmethod description :around ((parser parser))
  (if (slot-boundp parser 'name)
      (slot-value parser 'name)
      (call-next-method)))

(defmethod print-object ((object parser) stream)
  (format stream "~S" (description object)))

(defvar *nonterminal-source* nil)
(defvar *nonterminal-start* nil)
(defvar *nonterminal-end* nil)

(defgeneric parse (parser input))

(defmethod parse :around ((parser parser) input)
  (declare (ignore input))
  (if (slot-boundp parser 'action)
      (let* ((*nonterminal-source* (input-source input))
             (*nonterminal-start* (input-position input))
             (result (call-next-method))
             (*nonterminal-end* (input-position input)))
        (funcall (slot-value parser 'action) result))
      (call-next-method)))

(defun nonterminal-source ()
  (subseq *nonterminal-source* *nonterminal-start* *nonterminal-end*))

(define-condition parse-error (error)
  ((expected :initarg :expected)
   (actual :initarg :actual)
   (location :initarg :location))
  (:report (lambda (c s)
             (format s "Expected ~S~%Got ~S~:{~%At ~S in:~%~S~}"
                     (slot-value c 'expected)
                     (slot-value c 'actual)
                     (reverse (slot-value c 'location))))))

(defun parse-error (parser input)
  (multiple-value-bind (first valid) (peek input)
    (error 'parse-error
           :expected (description parser)
           :actual (if valid first :end-of-input)
           :location (list (input-location input)))))

(defun try-parse (parser input)
  (let ((old-position (input-position input)))
    (flet ((ignore-error-if-no-input-consumed (c)
             (when (= old-position (input-position input))
               (return-from try-parse (values c nil)))))
      (handler-bind ((parse-error #'ignore-error-if-no-input-consumed))
        (values (parse parser input) t)))))

(defclass anything-parser (parser)
  ())

(defmethod description ((parser anything-parser))
  '*anything*)

(defmethod parse ((parser anything-parser) input)
  (multiple-value-bind (token valid) (peek input)
    (when (not valid)
      (parse-error parser input))
    (advance input)
    token))

(defvar *anything* (make-instance 'anything-parser))

(defclass end-of-input-parser (parser)
  ())

(defmethod description ((parser end-of-input-parser))
  '*end-of-input*)

(defmethod parse ((parser end-of-input-parser) input)
  (unless (end-of-input-p input)
    (parse-error parser input)))

(defvar *end-of-input* (make-instance 'end-of-input-parser))

(defun parse-all (parser input)
  (prog1 (parse parser input)
    (parse *end-of-input* input)))

(defclass satisfies-parser (parser)
  ((predicate :initarg :predicate)))

(defmethod description ((parser satisfies-parser))
  (with-slots (predicate) parser
    (list 'satisfies predicate)))

(defmethod parse ((parser satisfies-parser) input)
  (with-slots (predicate) parser
    (multiple-value-bind (token valid) (peek input)
      (unless (and valid (funcall predicate token))
        (parse-error parser input))
      (advance input)
      token)))

(defun satisfies (predicate)
  (make-instance 'satisfies-parser :predicate predicate))

(defclass literal-parser (parser)
  ((value :initarg :value)
   (test :initarg :test
         :initform 'eql)))

(defmethod description ((parser literal-parser))
  (with-slots (value) parser
    (list 'literal value)))

(defmethod parse ((parser literal-parser) input)
  (with-slots (value test) parser
    (multiple-value-bind (token valid) (peek input)
      (unless (and valid (funcall test value token))
        (parse-error parser input))
      (advance input)
      token)))

(defun literal (value &key (test 'eql))
  (make-instance 'literal-parser :value value :test test))

(defclass sequence-parser (parser)
  ((parsers :initarg :parsers)))

(defmethod description ((parser sequence-parser))
  (with-slots (parsers) parser
    (list* 'sequence (mapcar #'description parsers))))

(defmethod parse ((parser sequence-parser) input)
  (with-slots (parsers) parser
    (mapcar (lambda (sub-parser) (parse sub-parser input)) parsers)))

(defun sequence (&rest parsers)
  (make-instance 'sequence-parser :parsers parsers))

(defclass alternative-parser (parser)
  ((parsers :initarg :parsers)))

(defmethod description ((parser alternative-parser))
  (with-slots (parsers) parser
    (list* 'alternative (mapcar #'description parsers))))

(defmethod parse ((parser alternative-parser) input)
  (with-slots (parsers) parser
    (dolist (sub-parser parsers)
      (multiple-value-bind (result valid)
          (try-parse sub-parser input)
        (when valid (return-from parse result))))
    (parse-error parser input)))

(defun alternative (&rest parsers)
  (make-instance 'alternative-parser :parsers parsers))

(defclass repeat-parser (parser)
  ((sub-parser :initarg :sub-parser)))

(defmethod description ((parser repeat-parser))
  (with-slots (sub-parser) parser
    (list 'repeat sub-parser)))

(defmethod parse ((parser repeat-parser) input)
  (with-slots (sub-parser) parser
    (loop for (result valid) = (multiple-value-list
                                (try-parse sub-parser input))
          while valid
          collecting result)))

(defun repeat (sub-parser)
  (make-instance 'repeat-parser :sub-parser sub-parser))

(defclass at-least-once-parser (parser)
  ((sub-parser :initarg :sub-parser)))

(defmethod description ((parser at-least-once-parser))
  (with-slots (sub-parser) parser
    (list 'at-least-once sub-parser)))

(defmethod parse ((parser at-least-once-parser) input)
  (with-slots (sub-parser) parser
    (loop for (result valid) = (list (parse sub-parser input) t)
            then (multiple-value-list
                  (try-parse sub-parser input))
          while valid
          collecting result)))

(defun at-least-once (sub-parser)
  (make-instance 'at-least-once-parser :sub-parser sub-parser))

(defclass optional-parser (parser)
  ((sub-parser :initarg :sub-parser)))

(defmethod description ((parser optional-parser))
  (with-slots (sub-parser) parser
    (list 'optional sub-parser)))

(defmethod parse ((parser optional-parser) input)
  (with-slots (sub-parser) parser
    (multiple-value-bind (result valid)
        (try-parse sub-parser input)
      (if valid result nil))))

(defun optional (sub-parser)
  (make-instance 'optional-parser :sub-parser sub-parser))

(defclass nested-parser (parser)
  ((sub-parser :initarg :sub-parser)))

(defmethod description ((parser nested-parser))
  (with-slots (sub-parser) parser
    (list 'nested sub-parser)))

(defmethod parse ((parser nested-parser) input)
  (with-slots (sub-parser) parser
    (multiple-value-bind (token valid) (peek input)
      (unless (and valid (typep token 'cl:sequence))
        (parse-error parser input))
      (let ((sub-input (make-input token)))
        ;; Ensure errors are propagated when a nested parser fails after
        ;; matching.
        (flet ((consume-input-if-sub-input-consumed (c)
                 (push (input-location input)
                       (slot-value c 'location))
                 (unless (zerop (input-position sub-input))
                   (advance input))))
          (handler-bind ((parse-error #'consume-input-if-sub-input-consumed))
            (prog1 (parse-all sub-parser sub-input)
              (advance input))))))))

(defun nested (sub-parser)
  (make-instance 'nested-parser :sub-parser sub-parser))

(defvar *nonterminals* (make-hash-table))

(defun nonterminal (name)
  (assert (symbolp name))
  (multiple-value-bind (parser valid)
      (gethash name *nonterminals*)
    (when (not valid)
      (error "Nonterminal ~S is not defined" name))
    parser))

(defun (setf nonterminal) (parser name)
  (assert (symbolp name))
  (assert (typep parser 'parser))
  (setf (gethash name *nonterminals*) parser))

(defmethod description ((parser symbol))
  (description (nonterminal parser)))

(defmethod parse ((parser symbol) input)
  (parse (nonterminal parser) input))

(defun %define-nonterminal (name parser)
  (assert (symbolp name))
  (assert (typep parser 'parser))
  (setf (slot-value parser 'name) name)
  (setf (nonterminal name) parser))

(defun %set-action (parser action)
  (setf (slot-value parser 'action) action)
  parser)

(defun booleanp (object)
  (or (eq object t) (eq object nil)))

(%define-nonterminal 'boolean (satisfies #'booleanp))
(%define-nonterminal 'character (satisfies #'characterp))
(%define-nonterminal 'keyword (satisfies #'keywordp))
(%define-nonterminal 'number (satisfies #'numberp))
(%define-nonterminal 'string (satisfies #'stringp))
(%define-nonterminal 'symbol (satisfies #'symbolp))

(%define-nonterminal
 'nonterminal-clause
 (alternative 'let-expr 'production-expr))

(%define-nonterminal
 'nonterminal-clauses
 (repeat 'nonterminal-clause))

(%define-nonterminal
 'let-expr
 (%set-action
  (nested (sequence (literal 'let)
                    (nested (repeat (nested (sequence 'symbol 'production-expr))))
                    (repeat *anything*)))
  (lambda (stuff)
    (destructuring-bind (keyword bindings body) stuff
      (declare (ignore keyword))
      (loop for (lhs rhs) in bindings
            for var = (or lhs (gensym))
            collecting var into vars
            collecting rhs into parsers
            when (not lhs)
              collect var into ignore
            finally
               (return
                 `(%set-action
                   (sequence ,@parsers)
                   (lambda (result)
                     (destructuring-bind ,vars result
                       ,@(when ignore
                           `((declare (ignore ,@ignore))))
                       ,@body)))))))))

(defvar *operators* (make-hash-table))

(defun %define-operator (name constructor)
  (assert (symbolp name))
  (setf (gethash name *operators*) constructor))

(%define-nonterminal
 'production-expr
 (alternative
  (%set-action
   (satisfies #'symbolp)
   (lambda (stuff)
     (if (keywordp stuff)
         `(literal ',stuff)
         `(quote ,stuff))))
  (%set-action
   (nested (sequence 'symbol (repeat 'production-expr)))
   (lambda (stuff)
     (destructuring-bind (operator args) stuff
     `(,(gethash operator *operators*) ,@args))))))

(%define-operator 'satisfies     'satisfies)
(%define-operator 'literal       'literal)
(%define-operator 'quote         'literal)
(%define-operator 'sequence      'sequence)
(%define-operator 'and           'sequence)
(%define-operator 'alternative   'alternative)
(%define-operator 'or            'alternative)
(%define-operator 'repeat        'repeat)
(%define-operator '*             'repeat)
(%define-operator 'at-least-once 'at-least-once)
(%define-operator '+             'at-least-once)
(%define-operator 'optional      'optional)
(%define-operator '?             'optional)
(%define-operator 'nested        'nested)

(defmacro define-nonterminal (name lambda-list &body clauses)
  (assert (symbolp name))
  (assert (null lambda-list))
  `(%define-nonterminal
    ',name
    ;; TODO: Grammar optimizations
    (alternative ,@(parse-all 'nonterminal-clauses (make-input clauses)))))
