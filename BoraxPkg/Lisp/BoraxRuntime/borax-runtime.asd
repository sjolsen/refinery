(defsystem "borax-runtime"
  :defsystem-depends-on ("borax-build/asdf-extensions")
  :class "borax-build/asdf-extensions:package-inferred-system-with-tests"
  :depends-on ("borax-runtime/memory" "borax-runtime/object-file")
  :tests (:borax-runtime/memory-test :borax-runtime/object-file-test))
