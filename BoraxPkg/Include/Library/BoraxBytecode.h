#ifndef BORAX_BYTECODE_H
#define BORAX_BYTECODE_H

/*
 * Why a bytecode interpreter?
 * ===========================
 *
 * Traditionally, the simplest way to interpret languages like lisp is to
 * implement a recursive evaluator:
 *
 *   eval :: Term -> Value
 *   eval (Number n)   = n
 *   eval (List (a:d)) = apply (eval a) (eval d)
 *   ...
 *
 * This requires that the implementation language support the semantics of the
 * target language. In cases where the target language has semantics not
 * supported directly by the implementation language, some form of translation
 * is necessary:
 *
 *   -- eval no longer returns a value, but now returns a transformed program
 *   -- that will compute a value given an initial state
 *
 *   type Store = String -> Value
 *
 *   eval :: Term -> State Store Value
 *   eval (List [Symbol "setq", Symbol x, e]) = do
 *     val <- eval e
 *     modify $ \lookup var -> if var == x then val else lookup var
 *     return val
 *   ...
 *
 * In our case, we're targeting C, where the following aspects of lisp are not
 * directly supported:
 *
 * - Automatic memory management
 * - Unbounded recursion
 * - Non-local exits
 * - Suspension of execution pending I/O
 *
 * We've already solved automatic memory management with the garbage
 * collector. Strictly speaking, we don't need to support unbounded recursion,
 * but we do at least need to avoid overflowing the C stack, and that rules out
 * a recursive evaluator.
 *
 * Non-local exits and suspension are where a direct evaluator would really fall
 * short. C does have longjmp, but that is not compatible with the semantics of
 * UNWIND-PROTECT, and doesn't allow us to handle illegal invocations gracefully
 * anyway. UEFI being a single-threaded environment is the final nail in the
 * coffin for a direct evaluator: we will have to implement context switching
 * ourselves, something we cannot do (portably) with the C stack.
 *
 * All of this points to program translation, breaking up lisp programs into
 * chunks that the interpreter can handle in bounded time, storing all state on
 * the heap so as to avoid storing any on the C stack. We have a choice between
 * a bytecode interpreter and a machine-code interpreter: both are feasible, but
 * the former is much simpler to implement and portable.
 *
 * The semantics of Common Lisp
 * ============================
 *
 * Up to this point we haven't explicitly nailed down the dialect of lisp we'll
 * be targeting. Since we're talking about semantics, though, we need to get
 * concrete. The obvious choices are Common Lisp and Scheme. Either would be a
 * fine choice; I've chosen to target Common Lisp since it's widely supported
 * and very well-documented, and because it does not support continuations with
 * indefinite extent -- and, frankly, because I'm more familiar with it.
 *
 * Choosing Common Lisp has some down-sides for us as implementers, particularly
 * as regards semantics. Scheme's semantics are designed to be as simple as
 * practical, and are formally documented in the Revised⁷ Report on
 * Scheme. Common Lisp's semantics are described more informally (or at least
 * more verbosely) in the Common Lisp HyperSpec, and are more geared toward
 * documenting existing practice as of the late 1980s and early 1990s than
 * chasing some platonic ideal of lisp-ness.
 *
 * Details of the semantics of Common Lisp relevant to the design of our
 * interpreter follow.
 *
 * Environments
 * ------------
 *
 * Common Lisp's evaluation model is based around three kinds of environment:
 *
 * 1. The global environment
 * 2. Dynamic environments
 * 3. Lexical environments
 *
 * All three kinds of environment are primarily concerned with the question,
 * "What entites exist, and how can they be named?" The kinds of environment
 * differ in when this question can be answered: the global environment answers
 * the question for entities that persist indefinitely and can be named at any
 * point in the program; the dynamic environment answers the question for
 * entities and names that only exist at run time; and the lexical environment
 * answers the question for the names that will be examined during program
 * analysis (i.e. compilation).
 *
 * Note that I said "entities" and "names," not "objects" and "symbols." I use
 * this non-standard terminology to highlight that we're talking about more than
 * just variables and their values -- but we'll get to that.
 *
 * The global environment is the simplest. It consists mainly of the default
 * bindings of the value and function slots of symbols, but also proclamations
 * and the registered names of packages, classes, and other defined
 * entities. The HyperSpec says:
 *
 *   "The global environment is that part of an environment that contains
 *    bindings with both indefinite scope and indefinite extent."
 *
 *     -- CLHS §3.1.1.1
 *
 * The dynamic environment consists primarily of the dynamically established
 * bindings of dynamic ("special") variables. It also includes information
 * related to non-local exits, namely which exits, handlers, and restarts are
 * active and how they can be named (e.g. via catch tags). The HyperSpec says:
 *
 *   "A dynamic environment for evaluation is that part of an environment that
 *    contains bindings whose duration is bounded by points of establishment and
 *    disestablishment within the execution of the form that established the
 *    binding."
 *
 *     -- CLHS §3.1.1.2
 *
 * The lexical environment maps names that appear in the program to their
 * corresponding bindings. It also includes declarations that affect that
 * mapping. The relevant bindings include variables, functions, macros, and
 * tags. The HyperSpec says:
 *
 *   "A lexical environment for evaluation at some position in a program is that
 *    part of the environment that contains information having lexical scope
 *    within the forms containing that position."
 *
 *     -- CLHS §3.1.1.3
 *
 * To summarize: the global and lexical environments play the same role in
 * Common Lisp as in just about any other language. The interesting bit is the
 * dynamic environment, which involves establishing and disestablishing bindings
 * and exits during program execution. A fair chunk of the design work ahead of
 * us lies in exactly how we establish and disestablish those bindings.
 *
 * Non-local exits
 * ---------------
 *
 * Common Lisp has three pairs of fundamental non-local control flow constructs:
 *
 * 1. BLOCK and RETURN-FROM
 * 2. TAGBODY and GO
 * 3. CATCH and THROW
 *
 * Block and catch forms are similar in that they evaluate to the value(s)
 * passed to the corresponding transfer operator. Block and tagbody forms are
 * similar in that their exits are bound lexically, while catch forms are bound
 * dynamically using catch tags. Tagbody forms are unique in that they can
 * establish multiple exits simultaneously.
 *
 * What are exits? Continuations, roughly; they are the points to which control
 * can be transferred with a return, go, or throw form:
 *
 *   (block hello
 *   ^ here
 *     (let ((x ...))
 *       ...
 *       (return-from hello 3)))
 *   => 3
 *
 *   (defun play (x)
 *     (tagbody
 *      point-1
 *        (when (< x 0)
 *        ^ here
 *          (throw 'hail-mary 6))
 *      point-2
 *        (decf x)
 *        ^ also here
 *        (go point-1)))
 *
 *   (catch 'hail-mary
 *   ^ here
 *     (play -1))
 *   => 6
 *
 * CATCH is not unique in establishing exits that can be called from other
 * contexts; the exits established by BLOCK and TAGBODY can be lexically
 * captured:
 *
 *   (block nil
 *     (mapcar #'(lambda (x) (when (< x 0) (return x))) '(-1 2 -3))
 *     nil)
 *   => -1
 *
 * An important property of exits that distinguishes them from general
 * continuations like those produced by Scheme's call/cc is that they become
 * invalid when their dynamic extent ends:
 *
 *   (let ((k (block nil (lambda (x) (return x)))))
 *     (if (numberp k)
 *         k
 *       (k 0)))
 *   => control error, not 0
 *
 * This is important because it ensures that all of the dynamic extents in a
 * Common Lisp system follow a last-in-first out discipline, and that we can
 * therefore implement exits and the rest of the dynamic environment using a
 * stack.
 *
 * One more thing to note about non-local exits: they can be intercepted by
 * UNWIND-PROTECT:
 *
 *   (let ((x 0))
 *     (catch 'end
 *       (unwind-protect
 *           (throw 'end nil)
 *         (setf x 1)))
 *     x)
 *   => 1
 *
 * The exact behavior of non-local exits is described by section 5.2 of the
 * HyperSpec. We will actually relax the handling of "abandoned" exit points,
 * allowing constructs like:
 *
 *   (catch 'alice
 *     (catch 'bob
 *       (unwind-protect
 *           (throw 'alice 0)
 *         (throw 'bob 1))))
 *   => 1
 *
 * SBCL does the same thing.
 *
 * Dynamic variable bindings
 * -------------------------
 *
 * Common Lisp allows for scoped overrides of variables declared "special:"
 *
 *   (let ((*standard-output* (make-string-output-stream)))
 *     (princ "hello")
 *     (get-output-stream-string *standard-output*))
 *   => "hello"
 *   ;; *standard-output* has been returned to its original value
 *
 * Note that the binding created by LET in this example is not lexically visible
 * to the definition of PRINC; the value of the variable is updated and read
 * dynamically. This effect also follows a stack discipline, and is undone even
 * if we exit the form that created the binding via a non-local exit (for
 * instance, if PRINC had called THROW).
 *
 * Lexical variable bindings
 * -------------------------
 *
 * Many lexical variables will only last until the end of their containing
 * scope. Some lexical variables, however, must have indefinite extent --
 * namely, those captured by closures. This prevents them from being allocated
 * on the stack. Their bindings must be heap-allocated instead.
 *
 * Multitasking
 * ------------
 *
 * Common Lisp does not define standard facilities for multitasking, but it is
 * an important feature for program organization that most implementations
 * provide. Our case is a bit special in that we don't have operating
 * system-provided thread libraries available to us. We will instead have to
 * build our multitasking cooperatively.
 *
 * This actually isn't too hard to do: we just need to make sure all I/O is
 * implemented in terms of a primitive operation that suspends the active task
 * until the result is available. We will extend this to cover operations like
 * sleep, yield, and channel communication. When all tasks are blocked on I/O or
 * timers, we exit the interpreter loop and use UEFI's built-in facilities to
 * wait on the appropriate device(s) before resuming execution.
 *
 * This approach does make poorly-coded programs susceptible to livelocks and
 * deadlocks; we won't attempt to detect or resolve those.
 *
 * The Borax bytecode interpreter
 * ==============================
 *
 * The interpeter relies heavily on the garbage-collected store we defined
 * earlier. The global environment is implemented entirely in terms of heap
 * objects. The interpreter uses pins to retain references to a handful of key
 * root objects like the package registry and the task list.
 *
 * The stack and registers
 * -----------------------
 *
 * Each task has a stack and a handful of registers, namely the base pointer
 * (BP), stack pointer (SP), values register (VR), and program counter (PC). The
 * stack is a simple vector of object references that are dynamically
 * partitioned into activation records. Each activation record corresponds to a
 * function call. An activation record is laid out as follows (with addresses
 * increasing upward):
 *
 *   SP -> +--------------------+
 *         |                    |
 *         |  Dynamic extents   |
 *         |                    |
 *         +--------------------+ <- Initial value of SP
 *         |                    |
 *         |   Local bindings   |
 *         |                    |
 *         +--------------------+
 *         |                    |
 *         |  Shared bindings   | -> Pointers to heap-allocated bindings
 *         |                    |
 *         +--------------------+
 *         |  Closure pointer   |
 *         +--------------------+
 *         |    Code pointer    |
 *         +--------------------|
 *         |      Saved PC      |
 *         +--------------------+
 *         |      Saved BP      |
 *   BP -> +--------------------+ <- Previous frame's SP
 *         | ////////////////// |
 *
 * On function entry, the caller's BP and PC are saved and the callee's code
 * pointer is pushed onto the stack. The interpreter populates the closure
 * pointer if applicable. Space is allocated for the shared and local bindings,
 * and the BP and SP are updated to point to the start and end of the frame,
 * respectively. Formal parameters are parsed and stored in the VR before the PC
 * is set to the function's entry point. All of this is done by the interpreter
 * based on the static data described in the code object and the parameters
 * passed by the caller.
 *
 * During function execution, dynamic extents are pushed and popped at the top
 * of the stack as the corresponding bindings are established and
 * disestablished. If a non-local exit is taken, it is first checked for
 * validity (exits are invalidated when their dynamic extent ends), then the
 * stack is unwound until the target exit or a cleanup is reached. Whichever is
 * reached first is popped and executed.
 *
 * Function return is implemented by restoring the saved PC and popping the
 * current activation record. Tail calls, which require that there be no dynamic
 * extents in the current activation record, are implemented by overwriting the
 * frame, retaining the saved BP and PC.
 *
 * Shared variable bindings
 * ------------------------
 *
 * Closures require the implementation of shared bindings. A given function
 * invocation will either receive shared bindings from another activation record
 * or produce shared bindings that will be closed over by other functions (or
 * both). The closure pointer implements the former, the shared bindings section
 * the latter.
 *
 * Consider the following functions:
 *
 *   (defun nested (x)
 *     (lambda ()
 *       (let (y)
 *         ...
 *         (lambda () (list x y)))))
 *
 *   (defun convoluted ()
 *     (let (x y z)
 *       ...
 *       (list (lambda () (list x y))
 *             (lambda () (list x z)))))
 *
 * In NESTED, the inner lambda needs to capture bindings from multiple
 * activation records. In CONVOLUTED, the closures of the two inner lambdas
 * overlap but are not identical.
 *
 * To support functions like NESTED, closure objects contain pointers to
 * multiple shared binding blocks. Instructions that access shared bindings
 * through the closure pointer will need to specify two indices: the block index
 * and the binding index.
 *
 * Supporting functions like CONVOLUTED does not require splitting up
 * locally-created shared bindings; CONVOLUTED could allocated one shared
 * binding block containing X, Y, and Z, and pass it to both nested
 * functions. The compiler is free to do this, for instance to improve locality,
 * but to avoid hanging on to dead locals the interpreter supports splitting
 * shared locals up into multiple blocks. These bindings are addressed with two
 * indices as well.
 *
 * Code objects
 * ------------
 *
 * Code objects represent compiled functions with a null closure. Non-trivial
 * closures are implemented as composite objects on top of code objects. Code
 * objects need to describe:
 *
 * - How formal parameters are to be handled
 * - How many shared and local bindings are required
 * - How dynamic extents should be reset and how result values should be bound
 *   upon entry to each basic block
 * - The instructions that make up each basic block
 * - Any constants referenced by the instructions
 * - Where execution should begin
 *
 * Clearly, bytecode needs to be stored in a vector specialized for byte
 * storage, and constants need to be stored in an object vector. Keeping the
 * formal parameter list as a lambda list is useful for diagnosing call errors,
 * so that is an object reference as well. The shared and local binding counts
 * affect how frames are parsed and need to be processed before any of the
 * bindings for the entry basic block anyway, so those are also separate
 * fields. The bindings themselves as well as extent resets are encoded in a
 * special instruction that can only occur at the beginning of a basic block.
 *
 * We also need objects corresponding to built-in functions implemented in
 * C. This works the same way as a regular code object, except that instead of a
 * bytecode array the code object holds a C function pointer. The PC, instead of
 * being a bytecode index, is a state machine index that gets passed in and out
 * of the C function.
 *
 * Including the entry point in the code object allows functions to share code
 * arrays. This is mainly useful for closure instruction locality.
 *
 * The Borax instruction set
 * =========================
 *
 * The Borax instruction set is designed around manipulating the interpreter
 * state described above. The breadth of data processing instructions like add,
 * shift, etc. typical of traditional architectures is left to functions.
 *
 * Note that the interpreter has no value stack or general-purpose
 * registers. Instead, the interpreter supports a number of addressing modes
 * that allow instructions to access stack bindings, constant data, and certain
 * kinds of heap data directly. The VR communicates data into and out of
 * functions and exits.
 *
 * Instruction syntax
 * ------------------
 *
 * The instruction syntax is specified in a variant of EBNF where:
 *
 * - Terminals that correspond to lisp symbols are written in upper-case without
 *   quotation marks;
 *
 * - Non-terminals are written in lower-case;
 *
 * - Concatenation is expressed by juxtaposition;
 *
 * - Multiple definitions of the same non-terminal indicate alternatives.
 *
 * Addressing modes
 * ----------------
 *
 *   index = natural-number;
 *
 * Indices are non-negative values used by the following addressing modes to
 * index into various data structures.
 *
 *   constant = "(" :CONSTANT index ")";
 *
 * Constants index into the constant data vector stored with each code object.
 *
 *   local = "(" :LOCAL index ")";
 *
 * Locals index into the local bindings section of the activation record.
 *
 *   shared = "(" :SHARED index index ")";
 *
 * The first index of a shared location is used to obtain a shared binding
 * object from the shared bindings section of the activation record. The second
 * index indexes into the shared binding object.
 *
 *   closure = "(" :CLOSURE index index ")";
 *
 * Closure locations work like shared locations, except that the shared binding
 * object is retrieved from the closure object instead of directly from the
 * activation record.
 *
 *   location = constant | local | shared | closure;
 *
 * For simplicity, only one syntactic category of location is defined. Not all
 * locations are semantically valid in all instructions.
 *
 * Control-flow instructions
 * -------------------------
 *
 *   values = :MULTIPLE-VALUES location | :VALUES { location };
 *
 * The values clause appears where an instruction reads or writes the values
 * register (VR). The first variant saves or loads the entire register into or
 * from a MULTIPLE-VALUES object. The second variant scatters or gathers values
 * into or from multiple locations. When neither variant is used, the VR is
 * unmodified.
 *
 *   condition = :IF [ :NOT ] location;
 *
 * The condition clause allows for conditional execution of an
 * instruction. Before any other operands are processed, the condition
 * location(s) are loaded and evaluated as a generalized boolean. If the
 * resulting value is NIL, the instruction is retired; otherwise, it is
 * processed as normal.
 *
 * Note that not all condition clauses are legal for all condition operations.
 *
 *   instruction = CALL [ :TAIL ] [ :FAST ] [ condition ] location [ values ];
 *
 * The CALL instruction calls an arbitrary callable object. If the :TAIL flag is
 * specified, a tail call is performed. If the :FAST flag is specified, the
 * values are bound directly by the target function; otherwise, they are parsed
 * according to the target function's lambda list.
 *
 * The callable object may be a symbol, in which case its SYMBOL-FUNCTION slot
 * is used.
 *
 *   instruction = JUMP [ condition ] index;
 *
 * The JUMP instruction transfers control to another instruction within the same
 * code object.
 *
 *   instruction = RETURN [ condition ] [ values ];
 *
 * The RETURN instruction performs a non-local exit using the saved PC slot in
 * the current activation record.
 *
 *   instruction = EXIT [ condition ] location [ values ];
 *
 * The EXIT instruction performs a non-local exit using the exit object stored
 * in the specified location.
 *
 *   instruction = THROW [ condition ] location [ values ];
 *
 * The THROW instruction performs a non-local exit using the catch tag stored in
 * the specified location.
 *
 * Dynamic extent operations
 * -------------------------
 *
 *   instruction = PUSH-SPECIAL location location;
 *
 * The PUSH-SPECIAL instruction establishes a new dynamic binding for the symbol
 * specified in the first location and initializes that binding with the value
 * loaded from the second location.
 *
 *   instruction = PUSH-EXIT location index;
 *
 * The PUSH-EXIT instruction establishes an exit to the code location specified
 * by index and stores the resulting exit object into the specified location.
 *
 *   instruction = PUSH-CATCH location index;
 *
 * The PUSH-EXIT instruction establishes an exit to the code location specified
 * by index and binds the resulting exit object to the tag specified in the
 * location.
 *
 *   instruction = PUSH-CLEANUP index;
 *
 * The PUSH-CLEANUP instruction establishes a cleanup at the code location
 * specified by index. When a cleanup is taken, there may be an in-flight exit;
 * if so, the exit and MULTIPLE-VALUES object containing the previous VR
 * contents are provided in the VR. Otherwise, these values are NIL.
 *
 *   instruction = POP-DYNAMIC natural-number;
 *
 * The POP-DYNAMIC instruction resets the dynamic extent depth to the specified
 * number. The POP-DYNAMIC instruction is intended to be used in the target
 * sequence a transfer of control that disestablish dynamic bindings.
 *
 * Data operations
 * ---------------
 *
 *   instruction = BIND values;
 *
 * The BIND instruction moves data from the VR into one or more addressable
 * locations. The BIND instruction is intended to be used in the target sequence
 * of a transfer of control and is suitable for storing formal parameters and
 * retrieving return and exit values.
 *
 *   shared-block = "(" :SHARED index ")";
 *   closure-block = "(" :CLOSURE index ")";
 *
 *   instruction = CAPTURE location location { shared-block | closure-block };
 *
 * The CAPTURE instruction loads a raw code object from the second location,
 * constructs a new closure object referencing that code object and the
 * specified binding blocks, and stores the resulting closure in the first
 * location.
 *
 *   instruction = MOVE [ condition ] location location;
 *
 * The MOVE instruction loads a value from the second location and stores it
 * into the first location.
 *
 * Instruction encoding
 * ====================
 *
 * The instruction encoding is byte-oriented and variable-width. Earlier bytes
 * in the instruction stream can affect how later bytes are interpreted. The
 * encoding is designed to be compact for common code sequences without
 * unnecessarily limiting programs.
 *
 * Variable-length integers
 * ------------------------
 *
 * Some fields are only three or four bits, which is not enough to store the
 * full range of values we'd like to support for things like argument count and
 * shared binding indices. The scheme used in such cases, called "field
 * encoding," follows:
 *
 * An N-bit field is interpreted as an unsigned integer. If the field value is
 * less than 2^(N-1), the encoded value is the field value. If the field value
 * is equal to 2^(N-1), the encoded value is the value of a continuation
 * byte.
 *
 * For simplicity, the value encoded by the continuation bytes is not biased, so
 * there are duplicate encodings of low values. Field-encoded values are thus
 * limited to the range 0-255.
 *
 * Future directions:
 *
 * - If needed, this encoding scheme could be extended by using 2^(N-2) to
 *   indicate one continuation byte and 2^(N-1) to indicate two. This would be a
 *   breaking change.
 *
 * Opcodes
 * -------
 *
 * Every instruction begins with a one-byte opcode. The top four bits of the
 * opcode identify the operation and, for CALL, the flags; the lower four bits
 * contain auxiliary encoding information.
 *
 *          OPCODE BYTE
 *                                             AUXILIARY FIELD
 *   Code  Operation         Aux
 *   ====  ================  ====            Aux  Interpretation
 *   0000  CALL              CNNN            ===  ===============
 *   0001  CALL :FAST        CNNN            0/1  Extended opcode
 *   0010  CALL :TAIL        CNNN             C   Condition flag
 *   0011  CALL :FAST :TAIL  CNNN             N   Operand count
 *                                            D   Depth
 *   0100  JUMP              CCCC
 *   0101  RETURN            CNNN
 *   0110  EXIT              CNNN             CONDITION FLAG (C)
 *   0111  THROW             CNNN
 *                                     Flag  Interpretation   Operands
 *   1000  PUSH-SPECIAL      0000      ====  ===============  ========
 *   1000  PUSH-EXIT         0001         0  Unconditional           0
 *   1000  PUSH-CATCH        0010         1  Boolean                 1
 *   1000  PUSH-CLEANUP      0011         2  Negated boolean         1
 *                                        3  ...
 *   1000  (unused)          0100
 *   ...   ...               ...
 *   1011  (unused)          1111              OPERAND COUNT (N)
 *
 *   1100  POP-DYNAMIC       DDDD       Count  Interpretation
 *   1101  BIND              NNNN       =====  =======================
 *   1110  CAPTURE           NNNN           0  (no values)
 *   1111  MOVE              CCCC           1  :MULTIPLE-VALUES l
 *                                      2 + N  :VALUES l_0 ... l_(N-1)
 *   <-- MSB              LSB -->
 *
 * The condition flag is one or four bits, depending on the operation. The
 * condition flag field is interpreted as an unsigned integer. Note that for
 * instructions with a one-bit field, the only available tests are unconditional
 * (i.e. no test) and boolean. The condition flag is not field-encoded. Non-zero
 * values of the condition flag result in additional operands being read for the
 * test.
 *
 * The depth field is field-encoded and is specific to the POP-DYNAMIC
 * instruction. It indicates the depth to which the interpreter should reset the
 * dynamic extent stack.
 *
 * The operand count is field-encoded and is interpreted in two ways. For the
 * CAPTURE instruction, the operand count is the number of block operands minus
 * one (there is no legal encoding for zero). For all other instructions, the
 * operand count encodes the kind and number of values operand(s). Note that an
 * operand count of 0 leaves the VR unmodified while an operand count of 2 makes
 * it empty.
 *
 * Operands
 * --------
 *
 * If the condition flag is present and non-zero, its value will dictate the
 * number of condition operands to load. These operands immediately follows the
 * opcode and any continuation bytes used to encode the operand count.
 *
 * After this are any fixed-function operands dictated by the operation,
 * followed by the remaining operands. Operands are encoded in three ways,
 * depending on the instruction: as index operands, as location operands, or as
 * binding block operands.
 *
 * Index operands are encoded without an addressing mode tag as two-byte
 * little-endian unsigned integers. These two-byte integers are not necessarily
 * aligned.
 *
 * Location operands are encoded with an addressing mode tag in the top two
 * bits:
 *
 *   Tag  Addressing mode
 *   ===  ===============
 *    00  Constant data
 *    01  Local binding
 *    10  Shared binding
 *    11  Closure binding
 *
 * For constant and local addressing modes, the remaining six bits are a
 * field-encoded index. For shared and closure bindings, the low six bits are
 * split into two three-bit fields: the higher three bits are the field-encoded
 * major index, identifying a binding block, and the lower three bits are the
 * field-encoded minor index, identifying a binding within the block. If both
 * fields require continuation bytes, the major index continuation byte comes
 * first.
 *
 * Binding block operands are encoded with a tag in the top bit:
 *
 *   Tag  Block type
 *   ===  ===============
 *     0  Shared binding
 *     1  Closure binding
 *
 * The remaining seven bits are a field-encoded index.
 *
 * Future directions:
 *
 * - Currently, code targets are always represented with two bytes. This could
 *   be compacted with a signed PC-relative variable-width encoding, but that's
 *   more complicated and somewhat difficult to take advantage of in the
 *   compiler, for relatively minimal gains.
 *
 * Example
 * -------
 *
 * As a worked example, here's what MAPCAR might look like specialized to one
 * input list:
 *
 *   (defun mapcar-1 (f l)
 *     (flet ((impl (l acc)
 *              (if l
 *                  (impl (cdr l) (cons (funcall f (car l)) acc))
 *                acc)))
 *       (nreverse (impl l nil))))
 *   =>
 *
 *   Name:      MAPCAR-1
 *   Arglist:   (f l)  ;; real MAPCAR would be (f &rest l)
 *   Local:     2
 *   Shared:    #(1)
 *   Constants: #(#<UNBOUND-LAMBDA 0x12341234> NIL NREVERSE)
 *   Entry:     0
 *   Code:      #(...)
 *
 *     D4 80 40      BIND :VALUES (:SHARED 0 0) (:LOCAL 0)
 *     E0 41 00 00   CAPTURE (:LOCAL 1) (:CONSTANT 0) (:SHARED 0)
 *     14 41 40 01   CALL :FAST (:LOCAL 1) :VALUES (:LOCAL 0) (:CONSTANT 1)
 *     20 02         CALL :TAIL (:CONSTANT 2)
 *
 *   Name:      #<UNBOUND-LAMBDA 0x12341234>
 *   Arglist:   (l acc)
 *   Local:     4
 *   Shared:    NIL
 *   Constants: #(#<UNBOUND-LAMBDA 0x12341234> BORAX-VM:CAR-CDR FUNCALL CONS)
 *   Entry:     13
 *   Code:      #(...)
 *
 *     D4 40 41      BIND :VALUES (:LOCAL 0) (:LOCAL 1)
 *     42 40 2A 00   JUMP :IF :NOT (:LOCAL 0) 42
 *     03 01 40      CALL (:CONSTANT 1) :VALUES (:LOCAL 0)
 *     D4 42 43      BIND :VALUES (:LOCAL 2) (:LOCAL 3)
 *     04 02 80 42   CALL (:CONSTANT 2) :VALUES (:SHARED 0 0) (:LOCAL 2)
 *     D3 42         BIND :VALUES (:LOCAL 2)
 *     04 03 42 41   CALL (:CONSTANT 3) :VALUES (:LOCAL 2) (:LOCAL 1)
 *     D3 41         BIND :VALUES (:LOCAL 1)
 *     34 00 43 41   CALL :FAST :TAIL (:CONSTANT 0)
 *                        :VALUES (:LOCAL 3) (:LOCAL 1)
 *     53 41         RETURN :VALUES (:LOCAL 1)
 *
 * A few things to note:
 *
 * - Fast calls are used where the target function is known statically, or where
 *   it is desirable to rely on the VM to handle argument parsing.
 *
 * - Functions can be referenced by name or by value.
 *
 * - The tail recursion in the local closure could (and arguably should) be a
 *   jump.
 *
 * - Conditional execution can't be negated. A call to NOT can fix this, or the
 *   JUMP above could be a conditional return.
 *
 * - The code for the two functions is stored in the same vector, taking up just
 *   44 bytes in total.
 *
 * It's also fairly hard to read. A better disassembly might look something
 * like:
 *
 *   ENTRY0   BIND (F L)
 *            CAPTURE IMPL #(#<UNBOUND-LAMBDA 0x12341234>) #(F)
 *            CALL :FAST IMPL (L NIL)
 *            CALL :TAIL NREVERSE
 *
 *   ENTRY1   BIND (L ACC)
 *            JUMP :IF (NOT L) LABEL0
 *            CALL BORAX-VM:CAR-CDR (L)
 *            BIND (L-CAR L-CDR)
 *            CALL FUNCALL (F L-CAR)
 *            BIND (L-CAR)
 *            CALL CONS (L-CAR ACC)
 *            BIND (ACC)
 *            CALL :FAST :TAIL #<UNBOUND-LAMBDA 0x12341234> (L-CDR ACC)
 *   LABEL0   RETURN (ACC)
 *
 * This is a non-trivial endeavour, especially when it comes to naming
 * temporaries and re-used locals.
 */

enum {
  // Control-flow operations
  BORAX_OPCODE_CALL,
  BORAX_OPCODE_JUMP,
  BORAX_OPCODE_RETURN,
  BORAX_OPCODE_EXIT,
  BORAX_OPCODE_THROW,
  // Dynamic extent operations
  BORAX_OPCODE_PUSH_SPECIAL,
  BORAX_OPCODE_PUSH_EXIT,
  BORAX_OPCODE_PUSH_CATCH,
  BORAX_OPCODE_PUSH_CLEANUP,
  BORAX_OPCODE_POP_DYNAMIC,
  // Data operations
  BORAX_OPCODE_BIND,
  BORAX_OPCODE_CAPTURE,
  BORAX_OPCODE_MOVE,
};

enum {
  // Addressing modes
  BORAX_MODE_CONSTANT,
  BORAX_MODE_LOCAL,
  BORAX_MODE_SHARED,
  BORAX_MODE_CLOSURE,
};

#endif // BORAX_BYTECODE_H
