(uiop:define-package :borax-virtual-machine
  (:nicknames :borax-vm)
  (:use-reexport :borax-virtual-machine/bytecode
                 :borax-virtual-machine/image
                 :borax-virtual-machine/initial-image
                 :borax-virtual-machine/object-file))

(in-package :borax-virtual-machine)
