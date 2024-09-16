(uiop:define-package :borax-virtual-machine/common-lisp
  (:nicknames :borax-vm/cl)
  (:use :uiop/common-lisp)
  (:shadow #:most-positive-fixnum #:most-negative-fixnum
           #:cons #:car #:cdr #:push
           #:t #:null #:nil #:list #:find
           #:class #:standard-class #:class-name #:find-class
           #:class-of #:typep
           #:package #:package-name #:find-package
           #:symbol #:symbol-package #:symbol-name #:symbol-value
           #:symbol-function #:find-symbol #:intern
           #:condition #:simple-condition #:serious-condition
           #:storage-condition #:cell-error #:undefined-function
           #:error #:program-error #:simple-error #:type-error
           #:character #:fixnum #:function
           #:eq #:setq
           #:simple-vector
           #:string #:string=)
  (:export #:most-positive-fixnum #:most-negative-fixnum
           #:cons #:car #:cdr #:push
           #:t #:null #:nil #:list #:find
           #:class #:standard-class #:class-name #:find-class
           #:class-of #:typep
           #:package #:package-name #:find-package
           #:symbol #:symbol-package #:symbol-name #:symbol-value
           #:symbol-function #:find-symbol #:intern
           #:condition #:simple-condition #:serious-condition
           #:storage-condition #:cell-error #:undefined-function
           #:error #:program-error #:simple-error #:type-error
           #:character #:fixnum #:function
           #:eq #:setq
           #:simple-vector
           #:string #:string=))

(in-package :borax-virtual-machine/common-lisp)
