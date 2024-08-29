(uiop:define-package :borax-virtual-machine/initial-image
  (:mix :closer-mop :uiop/common-lisp :borax-virtual-machine/image)
  (:use :borax-virtual-machine/object-file)
  (:shadow #:null #:nil)
  (:export #:make-initial-image))

(in-package :borax-virtual-machine/initial-image)

(defmethod get-object-section ((object record-object))
  (let ((class (class-of object)))
    (values 'object-section :size (+ 3 (length (record-slots class))))))

(defmethod write-object ((object record-object))
  (let ((class (class-of object)))
    (write-word #x07)  ; widetag object-record
    (write-word (length (record-slots class)))
    (write-word #x07)  ; unbound
    (loop for slot in (record-slots class)
          for value = (slot-value-using-class class object slot)
          do (write-translation value))))

(defclass null ()
  ()
  (:metaclass record-class))

(defclass root ()
  (nil
   numbers)
  (:metaclass record-class))

(defun make-initial-image ()
  (let ((root (make-instance 'root)))
    (with-slots (nil numbers) root
      (setf nil (make-instance 'null))
      (setf numbers nil)
      ;; reverse order
      (borax-vm/image:push 8675309 numbers)
      (borax-vm/image:push 343 numbers)
      (borax-vm/image:push 42 numbers)
      (loop for i from 5 downto 0
            do (borax-vm/image:push i numbers))
      root)))
