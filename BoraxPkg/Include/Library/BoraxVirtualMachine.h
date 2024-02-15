#ifndef BORAX_VIRTUAL_MACHINE_H
#define BORAX_VIRTUAL_MACHINE_H

/*
 * Why a virtual machine?
 * ======================
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
 * The Borax virtual machine
 * =========================
 *
 * Our virtual machine relies heavily on the garbage-collected store we defined
 * earlier. The global environment is implemented entirely in terms of heap
 * objects. The interpreter uses pins to retain references to a handful of key
 * root objects like the package registry and the task list.
 *
 * The stack and registers
 * -----------------------
 *
 * Each task has a stack and a handful of registers, namely the base pointer
 * (BP), stack pointer (SP), and program counter (PC). The stack is a simple
 * vector of object references that are dynamically partitioned into activation
 * records. Each activation record corresponds to a function call. An activation
 * record is laid out as follows (with addresses increasing upward):
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
 *         |  Closure pointer   |
 *         +--------------------|
 *         |    Default exit    |
 *         +--------------------+
 *         |    Code pointer    |
 *         +--------------------+
 *         |      Saved BP      |
 *   BP -> +--------------------+ <- Previous frame's SP
 *         | ////////////////// |
 *
 * On function entry, the caller's BP is saved and the callee's code pointer is
 * pushed onto the stack. The interpreter populates the default exit and closure
 * pointer, if applicable. Space is allocated for the local bindings (including
 * formal parameters), and the BP and SP are updated to point to the start and
 * end of the frame, respectively. All of this is done by the interpreter based
 * on the static data described in the code object and the parameters passed by
 * the caller.
 *
 * During function execution, dynamic extents are pushed and popped at the top
 * of the stack as the corresponding bindings are established and
 * disestablished. If a non-local exit is taken, it is first checked for
 * validity (exits are invalidated when their dynamic extent ends), then the
 * stack is unwound until the target exit or a cleanup is reached. If a cleanup
 * is reached, it is popped and called normally with a default exit
 * corresponding to the in-flight target exit.
 *
 * Function return is implemented by taking the default exit. Tail calls, which
 * require that there be no dynamic extents in the current activation record,
 * are implemented by overwriting the frame, retaining the saved BP and default
 * exit. To avoid allocating an exit descriptor for every call, return exits are
 * encoded as fixnum indices.
 *
 * Instruction encoding
 * --------------------
 *
 * Every instruction begins with an opcode byte. Most instructions take a fixed
 * number of arguments, which are encoded following the opcode. Some operations,
 * like function calls, support a variable number of arguments. In these cases,
 * the arguments are prefixed with a length, either alone or part of the opcode
 * itself.
 *
 * Rather than having a fixed set of registers or maintaining a value stack, the
 * virtual machine implements addressing modes allowing instructions to
 * reference local bindings, closure bindings, dynamic bindings, etc. Actual
 * arguments to function calls are specified with location references in the
 * call instruction. Formal parameters and results from function calls are
 * stored according to location references specified with each basic block.
 *
 * Under normal circumstances, the multiple-values mechanism is implemented
 * using the aforementioned location references, and the intermediate
 * representation of values is not visible to the program. There are situations
 * like cleanups and the implementation of MULTIPLE-VALUE-LIST that require
 * access to arbitrary values sequences, and there is a distinct addressing mode
 * for this.
 *
 * Code objects
 * ------------
 *
 * Code objects represent compiled functions with a null closure. Non-trivial
 * closures are implemented as composite objects on top of code objects. Code
 * objects need to describe:
 *
 * - How formal parameters are to be handled
 * - How many local bindings are required
 * - How dynamic extents should be reset and how result values should be bound
 *   upon entry to each basic block
 * - The instructions that make up each basic block
 * - Any constants referenced by the instructions
 *
 * Clearly, bytecode needs to be stored in a vector specialized for byte
 * storage, and constants need to be stored in an object vector. Keeping the
 * formal parameter list as a lambda list is useful for diagnosing call errors,
 * so that is an object reference as well. The local binding count affects how
 * frames are parsed and needs to be processed before any of the bindings for
 * the entry basic block anyway, so that is also a separate field. The bindings
 * themselves as well as extent resets are encoded in a special instruction that
 * can only occur at the beginning of a basic block.
 */

#endif // BORAX_VIRTUAL_MACHINE_H
