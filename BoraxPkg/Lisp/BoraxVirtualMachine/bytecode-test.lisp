(uiop:define-package :borax-virtual-machine/bytecode-test
  (:use :uiop/common-lisp :clunit
        :borax-virtual-machine/bytecode
        :borax-virtual-machine/image))

(in-package :borax-virtual-machine/bytecode-test)

(defsuite bytecode-example-suite ())

;; Same as initial-image
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

(defvar +sum-list-bytecode+
  #(#xD3 #x40            ;; 00  BIND ((:LOCAL 0))
    #xF0 #x41 #x00       ;; 02  MOVE (:LOCAL 1) (:CONSTANT 0)
    #x42 #x40 #x19 #x00  ;; 05  JUMP :IF (NOT (:LOCAL 0)) #x0019
    #x03 #x01 #x40       ;; 09  CALL (:CONSTANT 1) ((:LOCAL 0))
    #xD4 #x42 #x40       ;; 0C  BIND ((:LOCAL 2) (:LOCAL 0))
    #x04 #x02 #x41 #x42  ;; 0F  CALL (:CONSTANT 2) ((:LOCAL 1) (:LOCAL 2))
    #xD3 #x41            ;; 13  BIND ((:LOCAL 1))
    #x41 #x40 #x09 #x00  ;; 15  JUMP :IF (:LOCAL 0) #x09
    #x53 #x41))          ;; 19  RETURN ((:LOCAL 1))

(deftest mapcar-1-test (bytecode-example-suite)
  (with-image nil
    (let ((bf (reify (bytecode-function 'sum-list))))
      (assert-equalp +sum-list-bytecode+ (vector-data (bytecode bf)))
      (assert-equalp #(0 car-cdr +)      (bytecode-constants bf))
      (assert-equalp 3                   (bytecode-locals bf))
      (assert-equalp #()                 (bytecode-shared bf))
      (assert-equalp 'sum-list           (bytecode-name bf))
      (assert-equalp '(&rest l)          (bytecode-arglist bf))
      (assert-equalp 0                   (bytecode-entry bf)))))
