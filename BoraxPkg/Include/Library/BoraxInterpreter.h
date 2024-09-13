#ifndef BORAX_INTERPRETER_H
#define BORAX_INTERPRETER_H

#include <Library/BoraxMemory.h>

/*
 * Interpreter design
 * ==================
 *
 * The interpreter is responsible for progressing the state of the virtual
 * machine over time. The interpreter maintains information about running,
 * pending, and completed tasks; executes running tasks; and manages the
 * dispatching of I/O and built-in function calls.
 *
 * The interpreter is designed around two core assumptions:
 *
 *   1. The underlying UEFI implementation is single-threaded.
 *
 *   2. Applications are primarily I/O-bound.
 *
 * The interpreter accommodates these constraints with the following strategy:
 * priority is given to running tasks, which will eventually either complete or
 * attempt to perform I/O, at which point they will be marked pending. When all
 * tasks have reached the pending state, the pending I/O requests will be
 * submitted using the appropriate asynchronous UEFI APIs. When any I/O
 * completes, the corresponding task will be marked running and priority will
 * return to program execution.
 *
 * There are two possible ways to approach this design, corresponding to the two
 * modes of EFI_EVENT completion: the interpreter might implement a closed
 * interpreter loop, orchestrating event handling via callbacks and only exiting
 * once all tasks are complete; or the interpreter might have a one-shot
 * interpreter loop that terminates as soon as all tasks are pending, provides
 * the caller with the pending I/O requests, and relies on the caller to
 * re-activate the one-shot loop when I/O is completed. For flexibility, and to
 * permit applications to avoid the complexities of callbacks and cancellation,
 * the interpreter uses the latter model.
 *
 * The bytecode documentation describes how Lisp code calls into C; here we
 * detail the reverse. Because Lisp tasks do not use the C stack and may be
 * suspended for an arbitrarily long period pending I/O completion, C code
 * cannot call Lisp code synchronously. Instead, the C caller may spawn a task
 * and register an EFI_EVENT to be signalled on task completion.
 *
 * Memory safety
 * -------------
 *
 * UEFI does not provide a process abstraction that can be relied on to contain
 * memory access violations or clean up leaked resources. Between the
 * flexibility of Lisp and Borax's support for loading code from disk, it
 * behooves us then to take some steps to ensure that the execution of Lisp code
 * maintains the invariants the C language requires for well-definedness at the
 * very least. That is to say: it should not be possible to input data into the
 * Lisp application or inject Lisp code that will cause the virtual machine to
 * exhibit undefined behavior.
 *
 * The memory abstraction exposed by the garbage collector together with
 * defensive programming in the object file loader go a long way to ensuring
 * this. However, we must still take care in the implementation of the
 * interpreter not to introduce an opportunity for Lisp code to violate
 * assumptions made by the implementation. To this end, we use wherever possible
 * plain Lisp data structures to manage the state of the interpreter and
 * maintain a strict type-checking discipline. This is not the most efficient
 * possible way to implement the interpreter, but an inefficient implementation
 * that does not crash unpredictably is preferable to an efficient
 * implementation that does.
 *
 * Some interpreter state, however, cannot be represented safely using native
 * Lisp data structures. Take function pointers for example: if function
 * pointers were represented as Lisp integers, Lisp code could trivially
 * redirect a C call to an arbitrary location in memory. Such data must instead
 * be represented in memory that Lisp code cannot access, or at least cannot
 * mutate. In some cases it is sufficient to lock mutation on a standard Lisp
 * object; in other cases it may be necessary to manage the state outside the
 * Lisp allocator entirely and allow Lisp code to refer to that state only
 * indirectly through an opaque descriptor.
 *
 * This approach is suitable for core features like built-in function calls, but
 * may be too cumbersome for rapid prototyping against UEFI APIs. In that case,
 * we might provide an optional unsafe FFI component, not to be loaded by
 * default.
 *
 * Built-in functions
 * ------------------
 *
 * Built-in functions were outlined briefly in the documentation on the bytecode
 * virtual machine. Here, we go into more detail. Generally speaking, C function
 * calls are synchronous and involve creating activation records on the
 * stack. Both of these properties are incompatible with the execution model
 * described above. Built-in functions must be implemented in such a manner that
 * their state does not persist on the C stack indefinitely, and their dynamic
 * extent can span an exit from the interpreter into the I/O handler.
 *
 * Built-in functions, therefore, are implemented as state machines that store
 * non-transient state on the Lisp stack. A state transition is associated with
 * the execution of some C code that may use the C stack, but cleans up before
 * returning to the interpreter. There are a few different ways this kind of
 * state machine can be realized, but the approach we take is to associate each
 * built-in function with a C function that:
 *
 *   1. Receives access to the built-in function's stack frame;
 *
 *   2. Receives the previous state;
 *
 *   3. Provides the next state;
 *
 *   4. Implements the behavior of the built-in function piecewise, depending on
 *      the current state transition.
 *
 * This C function may affect the state of execution by, for example, returning,
 * calling another function, suspending the current task pending completion of
 * an I/O request, etc. A pointer to this function is wrapped together with
 * metadata needed to parse arguments, set up the stack frame, etc., in much the
 * same way as the bytecode array associated with a Lisp function.
 *
 * The parallels with bytecode execution are such that we refer to each state
 * transition of the underlying state machine as a "built-in function
 * pseudoinstruction" and treat the state value as if it were a program
 * counter. Note however that bytecode instructions cannot be interspersed with
 * built-in function pseudoinstructions, nor vice versa; that is what function
 * calls are for.
 *
 * Garbage collection
 * ------------------
 *
 * As noted in the memory system documentation, garbage collection must be
 * triggered when the amount of used memory exceeds some threshold. Triggering
 * garbage collection has major implications for the correctness of C code that
 * manipulates Lisp objects, so its timing must be predictable. The interpreter
 * is responsible for triggering garbage collection and does so during the
 * interpreter loop.
 *
 * This makes garbage collection predictable from the perspective of C code in
 * the calling context—the I/O handling code, for instance—but built-in function
 * frames that call back into Lisp may be active for an arbitrarily long amount
 * of time. Therefore, garbage collection may in general be triggered between
 * any two built-in function pseudoinstructions.
 *
 * Roughly speaking: C code should make the conservative assumption that the
 * garbage collector may be triggered after every bytecode instruction and every
 * built-in function pseudoinstruction.
 *
 * Interpreter lifetime
 * --------------------
 *
 * The interpreter must be constructed using a pre-initialized global
 * environment object. This object is intended to be retrieved from a pre-built
 * Lisp image using the object file loader. The interpreter takes ownership of
 * the pin on successful construction.
 *
 * The interpreter's internal resources must ultimately be cleaned up by calling
 * BoraxInterpreterCleanup. This may be done at any time, but will prevent any
 * task unwinding code from running; callers of BoraxInterpreterSpawn will still
 * receive their completion events with data indicating an abnormal task
 * exit. Once BoraxInterpreterCleanup is called, no accesses to the interpreter
 * API are permitted, including from completion callbacks initated by the
 * cleanup process.
 *
 * To allow tasks to unwind gracefully, call BoraxInterpreterShutdown first. The
 * interpreter loop may continue to yield I/O requests indefinitely as tasks are
 * merely signalled to exit, not forced to do so. If the interpreter loop yields
 * no I/O requests, all tasks have exited. The interpreter is still fully
 * operational in this state and new tasks can be spawned. At any point,
 * BoraxInterpreterCleanup may be called to terminate all tasks forcefully.
 *
 * Tasks can be spawned with BoraxInterpreterSpawn. The optional completion
 * event will be signalled when the task exits, either normally or
 * forcefully. The caller may optionally take a reference to the created task
 * for later retrieval of its result; the caller is responsible for releasing
 * this pin reference.
 *
 * Condition handling
 * ------------------
 *
 * Various code paths in pseudoinstructions and the interpreter itself may need
 * to signal a condition. To avoid C recursion in handling these conditions,
 * pseudoinstructions and task-level interpreter APIs may indicate a condition
 * via the return value, which is otherwise NIL. Such conditions are always
 * signalled as if by ERROR: they cannot be handled except by non-local control
 * transfer, and if not handled will result in the task calling INVOKE-DEBUGGER.
 *
 * The process of signalling an interpreter-level error condition may itself
 * encounter an error, for instance if stack memory is exhausted by the
 * signalling process. In such a double-fault state, the task exits immediately.
 *
 * Memory exhaustion conditions in particular must be handled carefully: these
 * conditions are encoded as immediates to avoid attempting to allocate in a
 * situation where allocation has already failed, but this prevents us from
 * reporting any detailed information in these situations. The stack guard is
 * disabled when signalling a stack exhaustion condition for this reason.
 *
 * The result of a condition-valued function is interpreted as follows:
 *
 *   - NIL: no condition should be signalled
 *   - A condition object: passed as the datum argument to ERROR
 *   - A condition class: passed as the datum argument to ERROR
 *
 * To support these semantics, Borax extends the concept of condition
 * designators (CLHS §9.1.2.1) to include condition class objects.
 */

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Packages;
} BORAX_GLOBAL_ENVIRONMENT;

typedef enum {
  // Standard packages
  BORAX_GLOBAL_PACKAGE_COMMON_LISP,
  BORAX_GLOBAL_PACKAGE_KEYWORD,
  // Built-in packages
  BORAX_GLOBAL_PACKAGE_BORAX_RUNTIME,
  // Standard conditions
  BORAX_GLOBAL_CLASS_SIMPLE_ERROR,
  BORAX_GLOBAL_CLASS_SIMPLE_PROGRAM_ERROR,
  BORAX_GLOBAL_CLASS_TYPE_ERROR,
  BORAX_GLOBAL_CLASS_UNDEFINED_FUNCTION,
  // Built-in conditions
  BORAX_GLOBAL_CLASS_HEAP_EXHAUSTED,
  BORAX_GLOBAL_CLASS_STACK_EXHAUSTED,
  BORAX_GLOBAL_CLASS_LOCATION_ERROR,
  // Standard classes
  BORAX_GLOBAL_CLASS_FUNCTION,
  BORAX_GLOBAL_CLASS_STRING,
  BORAX_GLOBAL_CLASS_SYMBOL,
  // Built-in classes
  BORAX_GLOBAL_CLASS_BYTECODE_FUNCTION,
  // Keyword symbols
  BORAX_GLOBAL_KEYWORD_CONSTANT,
  BORAX_GLOBAL_KEYWORD_LOCAL,
  BORAX_GLOBAL_KEYWORD_SHARED,

  BORAX_GLOBAL_COUNT
} BORAX_GLOBAL;

typedef struct {
  BORAX_PIN_RECORD            Record;
  BORAX_ALLOCATOR             *Alloc;
  BORAX_GLOBAL_ENVIRONMENT    *GlobalEnvironment;
  UINTN                       GcPageThreshold;
  LIST_ENTRY                  TaskList;
  BORAX_OBJECT                Globals[BORAX_GLOBAL_COUNT];
} BORAX_INTERPRETER;

typedef struct _BORAX_TASK BORAX_TASK;

EFI_STATUS
EFIAPI
BoraxInterpreterInit (
  IN BORAX_ALLOCATOR     *Alloc,
  IN BORAX_OBJECT        GlobalEnvironment,
  OUT BORAX_INTERPRETER  **Interp
  );

VOID
EFIAPI
BoraxInterpreterCleanup (
  IN BORAX_INTERPRETER  *Interp
  );

EFI_STATUS
EFIAPI
BoraxInterpreterSpawn (
  IN BORAX_INTERPRETER  *Interp,
  IN EFI_EVENT          Completion    OPTIONAL,
  IN BORAX_OBJECT       ErrorHandler  OPTIONAL,
  IN BORAX_OBJECT       EntryPoint,
  IN BORAX_OBJECT       Args,
  OUT BORAX_TASK        **Task        OPTIONAL
  );

VOID
EFIAPI
BoraxInterpreterRun (
  IN BORAX_INTERPRETER  *Interp,
  OUT BORAX_PIN         **IORequests
  );

VOID
EFIAPI
BoraxInterpreterShutdown (
  IN BORAX_INTERPRETER  *Interp
  );

typedef union {
  BORAX_OBJECT_HEADER    Header;
  struct {
    UINTN           Word0;
    UINTN           Capacity;
    UINTN           Length;
    BORAX_OBJECT    Values[];
  };
} BORAX_MULTIPLE_VALUES;

EFI_STATUS
EFIAPI
BoraxMakeMultipleValues (
  IN BORAX_INTERPRETER       *Interp,
  IN UINTN                   Length,
  OUT BORAX_MULTIPLE_VALUES  **Values
  );

EFI_STATUS
EFIAPI
BoraxResizeMultipleValues (
  IN BORAX_INTERPRETER          *Interp,
  IN OUT BORAX_MULTIPLE_VALUES  **Values,
  IN UINTN                      Length
  );

typedef enum {
  BORAX_TASK_STARTING,
  BORAX_TASK_RUNNING,
  BORAX_TASK_PENDING,
  BORAX_TASK_RETURNED,
  BORAX_TASK_ABORTED,
  BORAX_TASK_DOUBLE_FAULTED,
} BORAX_TASK_STATE;

typedef struct {
  BORAX_OBJECT    **Pages;
  UINTN           PagesLength;
  UINTN           PagesCapacity;
} BORAX_TASK_STACK;

typedef struct {
  UINTN                    BP;
  UINTN                    SP;
  UINTN                    PC;
  UINTN                    LC;
  UINTN                    SC;
  BORAX_MULTIPLE_VALUES    *VR;
} BORAX_TASK_REGISTERS;

struct _BORAX_TASK {
  BORAX_PIN_RECORD        Record;
  LIST_ENTRY              TaskList;
  BORAX_INTERPRETER       *Interp;
  BORAX_TASK_STATE        State;
  BORAX_TASK_STACK        Stack;
  BORAX_TASK_REGISTERS    Registers;
  BORAX_OBJECT            ErrorHandler;
  BORAX_OBJECT            EntryPoint;
  EFI_EVENT               Completion;

  union {
    BORAX_OBJECT    AbortCondition;
    struct {
      BORAX_OBJECT    Condition1;
      BORAX_OBJECT    Condition2;
    } DoubleFault;
  };
};

BORAX_OBJECT *
EFIAPI
BoraxTaskUnsafeStackAddress (
  IN BORAX_TASK  *Task,
  IN UINTN       Index
  );

BORAX_OBJECT
EFIAPI
BoraxTaskAccessLocal (
  IN BORAX_TASK     *Task,
  IN UINTN          Index,
  OUT BORAX_OBJECT  **Local
  );

BORAX_OBJECT
EFIAPI
BoraxTaskReadConstant (
  IN BORAX_TASK     *Task,
  IN UINTN          Index,
  OUT BORAX_OBJECT  *Constant
  );

BORAX_OBJECT
EFIAPI
BoraxTaskBind (
  IN BORAX_TASK     *Task,
  IN UINTN          SlotsLength,
  OUT BORAX_OBJECT  **Slots
  );

BORAX_OBJECT
EFIAPI
BoraxTaskCoBind (
  IN BORAX_TASK    *Task,
  IN UINTN         SlotsLength,
  IN BORAX_OBJECT  *Slots
  );

BORAX_OBJECT
EFIAPI
BoraxTaskEnterFunction (
  IN BORAX_TASK    *Task,
  IN BORAX_OBJECT  Function
  );

BORAX_OBJECT
EFIAPI
BoraxTaskEnterFunctionTail (
  IN BORAX_TASK    *Task,
  IN BORAX_OBJECT  Function
  );

BORAX_OBJECT
EFIAPI
BoraxTaskExitFunction (
  IN BORAX_TASK  *Task
  );

typedef union {
  BORAX_OBJECT_HEADER    Header;
  struct {
    BORAX_HALFWORD    HalfWord0;
    BORAX_HALFWORD    Valid;
    BORAX_OBJECT      Task;
    UINTN             BP;
    UINTN             PC;
  };
} BORAX_EXIT;

BORAX_OBJECT
EFIAPI
BoraxTaskPushExit (
  IN BORAX_TASK   *Task,
  IN UINTN        PC,
  OUT BORAX_EXIT  **Exit
  );

BORAX_OBJECT
EFIAPI
BoraxTaskTakeExit (
  IN BORAX_TASK    *Task,
  IN BORAX_OBJECT  Exit
  );

BORAX_OBJECT
EFIAPI
BoraxTaskPopDynamic (
  IN BORAX_TASK    *Task,
  IN UINTN         Depth,
  IN BORAX_OBJECT  TargetExit,
  OUT BOOLEAN      *Intercepted
  );

BORAX_OBJECT
EFIAPI
BoraxTaskError (
  IN BORAX_TASK    *Task,
  IN BORAX_OBJECT  Condition
  );

VOID
EFIAPI
BoraxTaskAbort (
  IN BORAX_TASK    *Task,
  IN BORAX_OBJECT  Condition
  );

typedef struct {
  UINTN           BP;
  UINTN           SP;
  UINTN           PC;
  BORAX_OBJECT    Code;
} BORAX_STACK_FRAME;

typedef struct {
  BORAX_TASK    *Task;
  UINTN         NextBP;
  UINTN         NextSP;
  UINTN         NextPC;
} BORAX_STACK_FRAME_ITERATOR;

BORAX_STACK_FRAME_ITERATOR
EFIAPI
BoraxStackFrameIterate (
  IN BORAX_TASK  *Task
  );

BOOLEAN
EFIAPI
BoraxStackFrameNext (
  IN BORAX_STACK_FRAME_ITERATOR  *Iter,
  OUT BORAX_STACK_FRAME          *Frame
  );

VOID
EFIAPI
BoraxTaskDebugStackTrace (
  IN UINTN       ErrorLevel,
  IN BORAX_TASK  *Task
  );

typedef
BORAX_OBJECT
(EFIAPI *BORAX_FUNCTION_OP_RUN)(
  IN BORAX_TASK    *Task,
  IN BORAX_OBJECT  Function
  );

typedef struct {
  enum {
    BORAX_FUNCTION_NAME_NONE = 0,
    BORAX_FUNCTION_NAME_C_STRING,
    BORAX_FUNCTION_NAME_OBJECT,
  } Tag;
  union {
    CONST CHAR16    *CString;
    BORAX_OBJECT    Object;
  };
} BORAX_FUNCTION_NAME;

typedef
BORAX_OBJECT
(EFIAPI *BORAX_FUNCTION_OP_NAME)(
  IN BORAX_INTERPRETER     *Interp,
  IN BORAX_OBJECT          Function,
  OUT BORAX_FUNCTION_NAME  *Name
  );

typedef struct {
  UINTN    Entry;
  UINTN    Locals;
  UINTN    Shared;
} BORAX_FUNCTION_INFO;

typedef
BORAX_OBJECT
(EFIAPI *BORAX_FUNCTION_OP_INFO)(
  IN BORAX_INTERPRETER     *Interp,
  IN BORAX_OBJECT          Function,
  OUT BORAX_FUNCTION_INFO  *Info
  );

typedef
BORAX_OBJECT
(EFIAPI *BORAX_FUNCTION_OP_SHARED)(
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Function,
  IN UINTN              Block,
  OUT UINTN             *Count
  );

typedef
BORAX_OBJECT
(EFIAPI *BORAX_FUNCTION_OP_CONSTANT)(
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Function,
  IN UINTN              Index,
  OUT BORAX_OBJECT      *Constant
  );

typedef struct {
  BORAX_FUNCTION_OP_RUN         Run;
  BORAX_FUNCTION_OP_NAME        Name;
  BORAX_FUNCTION_OP_INFO        Info;
  BORAX_FUNCTION_OP_SHARED      Shared;
  BORAX_FUNCTION_OP_CONSTANT    Constant;
} BORAX_FUNCTION_OPS;

extern CONST BORAX_FUNCTION_OPS  gBuiltInFunctionOps;
extern CONST BORAX_FUNCTION_OPS  gBytecodeFunctionOps;

BORAX_OBJECT
EFIAPI
BoraxResolveFunction (
  IN BORAX_INTERPRETER          *Interp,
  IN OUT BORAX_OBJECT           *Function,
  OUT CONST BORAX_FUNCTION_OPS  **Ops
  );

typedef
BORAX_OBJECT
(EFIAPI *BORAX_BUILT_IN_CODE)(
  IN BORAX_TASK *Task
  );

typedef union {
  BORAX_OBJECT_HEADER    Header;
  struct {
    UINTN                  Word0;
    CONST CHAR16           *Name;
    BORAX_OBJECT           Arglist;
    UINTN                  Entry;
    BORAX_BUILT_IN_CODE    Code;
    UINTN                  Locals;
    UINTN                  SharedLength;
    CONST UINTN            *Shared;
    UINTN                  ConstantsLength;
    BORAX_OBJECT           Constants[];
  };
} BORAX_BUILT_IN_FUNCTION;

EFI_STATUS
EFIAPI
BoraxMakeBuiltInFunction (
  IN BORAX_ALLOCATOR           *Alloc,
  IN CONST CHAR16              *Name,
  IN BORAX_OBJECT              Arglist,
  IN UINTN                     Entry,
  IN BORAX_BUILT_IN_CODE       Code,
  IN UINTN                     Locals,
  IN UINTN                     SharedLength,
  IN UINTN                     *Shared  OPTIONAL,
  IN UINTN                     ConstantsLength,
  OUT BORAX_BUILT_IN_FUNCTION  **Function
  );

typedef union {
  BORAX_OBJECT_HEADER    Header;
  struct {
    UINTN    Word0;
    UINTN    Size;
    UINT8    Data[];
  };
} BORAX_CONSTANT;

#define BORAX_CONSTANT_SIZE(_type)  (sizeof (_type) - sizeof (BORAX_CONSTANT))

EFI_STATUS
EFIAPI
BoraxMakeConstant (
  IN BORAX_ALLOCATOR  *Alloc,
  IN UINTN            Size,
  OUT BORAX_CONSTANT  **Constant
  );

#endif // BORAX_INTERPRETER_H
