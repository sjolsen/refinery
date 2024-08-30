(defsystem "borax-virtual-machine"
  :defsystem-depends-on ("borax-build/asdf-extensions")
  :class "borax-build/asdf-extensions:package-inferred-system-with-tests"
  :components ((:file "package"))
  :depends-on ("borax-virtual-machine/common-lisp"
               "borax-virtual-machine/image"
               "borax-virtual-machine/initial-image"
               "borax-virtual-machine/object-file")
  :tests (:borax-virtual-machine/image-test :borax-virtual-machine/object-file-test))
