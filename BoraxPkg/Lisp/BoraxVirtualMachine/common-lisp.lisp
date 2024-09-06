(uiop:define-package :borax-virtual-machine/common-lisp
  (:nicknames :borax-vm/cl)
  (:use :uiop/common-lisp)
  (:shadow #:most-positive-fixnum #:most-negative-fixnum
           #:cons #:car #:cdr #:push
           #:null #:nil
           #:class #:standard-class #:find-class
           #:package #:package-name #:find-package
           #:symbol #:symbol-package #:symbol-name #:symbol-value
           #:find-symbol #:intern
           #:setq
           #:simple-vector
           #:string #:string=)
  (:export #:most-positive-fixnum #:most-negative-fixnum
           #:cons #:car #:cdr #:push
           #:null #:nil
           #:class #:standard-class #:find-class
           #:package #:package-name #:find-package
           #:symbol #:symbol-package #:symbol-name #:symbol-value
           #:find-symbol #:intern
           #:setq
           #:simple-vector
           #:string #:string=))

(in-package :borax-virtual-machine/common-lisp)
