(uiop:define-package :borax-virtual-machine/common-lisp
  (:nicknames :borax-vm/cl)
  (:use :uiop/common-lisp)
  (:shadow #:most-positive-fixnum #:most-negative-fixnum
           #:cons #:car #:cdr #:push
           #:null #:nil
           #:class
           #:string)
  (:export #:most-positive-fixnum #:most-negative-fixnum
           #:cons #:car #:cdr #:push
           #:null #:nil
           #:class
           #:string))

(in-package :borax-virtual-machine/common-lisp)
