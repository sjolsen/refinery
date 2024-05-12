(defsystem "borax-virtual-machine"
  :defsystem-depends-on ("borax-build/asdf-extensions")
  :class "borax-build/asdf-extensions:package-inferred-system-with-tests"
  :depends-on ("borax-virtual-machine/memory" "borax-virtual-machine/object-file")
  :tests (:borax-virtual-machine/memory-test :borax-virtual-machine/object-file-test))
