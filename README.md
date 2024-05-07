Refinery
========

Refinery is a work-in-progress application for exploring UEFI environments. It
is currently a very simple C application amounting to a VGA text-mode "hello
world." I plan to replace the C application with a Common Lisp application
before developing it further.

Common Lisp is not a natively supported language for UEFI systems. I'm designing
and implementing a lisp virtual machine tailored to UEFI, partly to avoid
writing large amounts of asynchronous, tediously error-checked C, and partly for
the fun of implementing an entire language stack.

Borax
-----

The lisp implementation for Refinery is named "Borax" after the mineral used as
a flux in metallurgy. The design of Borax is itself in flux (😉) but at a high
level the plan is to organize the implementation into a virtual machine with two
implementations: one in C and one in lisp. This should enable developing as much
of the system as possible in lisp itself, with the aid of an existing hosted
lisp implementation (in practice, SBCL).

The design, such as it currently is, is explained mainly through the comments in
the header files under BoraxPkg/Include/Library. To briefly summarize:

1. `BoraxMemory.h` describes the design and implementation of the allocator and
   garbage collector.
2. `BoraxObjectFile.h` describes the ELF-like intermediate format that will be
   used to prepare and load lisp images and compiled files.
3. `BoraxVirtualMachine.h` describes the bytecode language the virtual machine
   will interpret (the current plan is for Borax to be a compiled-only
   implementation).

Building and testing
--------------------

Refinery is a UEFI application written against the TianoCore EDK II API. The C
components of Refinery and Borax are only designed to be buildable by the edk2
build system. To this end, the
[uefi-workspace](https://github.com/sjolsen/uefi-workspace) repository vendors
the refinery repository, edk2, and in future any other build dependencies. The
uefi-workspace repository also contains scripts for setting up the development
environment, building the software, and running tests.
