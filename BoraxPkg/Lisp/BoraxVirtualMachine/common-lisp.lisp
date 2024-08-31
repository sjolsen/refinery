(uiop:define-package :borax-virtual-machine/common-lisp
  (:nicknames :borax-vm/cl)
  (:use :uiop/common-lisp)
  (:shadow #:most-positive-fixnum #:most-negative-fixnum
           #:cons #:car #:cdr #:push
           #:null #:nil
           #:class #:standard-class
           #:package #:package-name
           #:symbol #:symbol-package #:symbol-name
           #:string)
  (:export #:most-positive-fixnum #:most-negative-fixnum
           #:cons #:car #:cdr #:push
           #:null #:nil
           #:class #:standard-class
           #:package #:package-name
           #:symbol #:symbol-package #:symbol-name
           #:string))

(in-package :borax-virtual-machine/common-lisp)
