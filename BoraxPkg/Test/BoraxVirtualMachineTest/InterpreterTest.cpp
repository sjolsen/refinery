#include "MemoryTest.hpp"

#include <optional>
#include <vector>

extern "C" {
  #include <Library/BoraxInterpreter.h>
  #include <Library/BoraxObjectFile.h>
  #include <Library/BoraxPlugin.h>
  #include <Library/BoraxRuntime.h>
}

#include "BoraxVirtualMachineTest.hpp"
#include "Error.hpp"
#include "MockEvent.hpp"
#include "MockFile.hpp"

using namespace borax::testing;

struct InterpreterDeleter {
  void
  operator() (
    BORAX_INTERPRETER  *Interp
    )
  {
    BoraxInterpreterCleanup (Interp);
  }
};

using AutoInterpreter = std::unique_ptr<BORAX_INTERPRETER, InterpreterDeleter>;

struct TaskDeleter {
  void
  operator() (
    BORAX_TASK  *Task
    )
  {
    BoraxReleasePinRecord (&Task->Record);
  }
};

using AutoTask = std::unique_ptr<BORAX_TASK, TaskDeleter>;

class TaskAbortedError : public ConditionError {
public:
  TaskAbortedError (
                    IN BORAX_ALLOCATOR  *Alloc,
                    IN BORAX_OBJECT     Condition
                    ) : ConditionError (Alloc, Condition)
  {
  }
};

class TaskDoubleFaultedError : public ConditionError {
protected:
  AutoPin Condition2_;
public:
  TaskDoubleFaultedError (
                          IN BORAX_ALLOCATOR  *Alloc,
                          IN BORAX_OBJECT     Condition1,
                          IN BORAX_OBJECT     Condition2
                          ) : ConditionError (Alloc, Condition1),
    Condition2_ (PinCondition (Alloc, Condition2))
  {
  }

  BORAX_OBJECT
  Condition2 (
    ) const noexcept
  {
    return Condition2_->Object;
  }
};

class InterpreterTests : public MemoryTests {
public:
  MockEventEngine EventEngine;
  AutoInterpreter Interp;

  AutoPin
  LoadImage (
    )
  {
    EFI_STATUS  Status;
    PosixFile   File { TestBase / "InitialImage.bxo" };
    BORAX_PIN   *Pin;

    Status = BoraxLoadObjectFile (&Alloc, File.GetProtocol (), &Pin);
    if (EFI_ERROR (Status)) {
      throw EFIError { Status };
    }

    return AutoPin { Pin };
  }

  void
  SetUp (
    ) override
  {
    EFI_STATUS         Status;
    BORAX_INTERPRETER  *Interp;
    AutoPin            Pin;

    MemoryTests::SetUp ();

    Pin = LoadImage ();

    Status = BoraxInterpreterInit (&Alloc, Pin->Object, &Interp);
    if (EFI_ERROR (Status)) {
      throw EFIError { Status };
    }

    this->Interp = AutoInterpreter { Interp };
  }

  void
  TearDown (
    ) override
  {
    this->Interp.reset ();

    MemoryTests::TearDown ();
  }

  void
  AddPlugin (
    IN CONST BORAX_DESCRIPTOR_PLUGIN  *Plugin,
    IN std::vector<BORAX_OBJECT>      Data = { }

    )
  {
    BORAX_OBJECT       Condition;
    BORAX_PLUGIN_DATA  DataList = {
      .Length = Data.size (),
      .Values = Data.data (),
    };

    Condition = BoraxAddPlugin (Interp.get (), Plugin, &DataList);
    if (BORAX_BOOL (Condition)) {
      throw ConditionError { Interp->Alloc, Condition };
    }
  }

  BORAX_SYMBOL *
  Intern (
    IN const wchar_t  *Package,
    IN const wchar_t  *Name
    )
  {
    BORAX_OBJECT  Condition;
    BORAX_SYMBOL  *Symbol;

    BORAX_DESCRIPTOR_SYMBOL  Desc = {
      .Package = reinterpret_cast<CONST CHAR16 *>(Package),
      .Name    = reinterpret_cast<CONST CHAR16 *>(Name),
    };

    Condition = BoraxIntern (Interp.get (), &Desc, &Symbol);
    if (BORAX_BOOL (Condition)) {
      throw ConditionError { Interp->Alloc, Condition };
    }

    return Symbol;
  }

  BORAX_CONST_STRING
  String (
    IN const wchar_t  *Str
    )
  {
    BORAX_CONST_STRING  Desc;

    Desc.Data   = reinterpret_cast<CONST CHAR16 *>(Str);
    Desc.Length = StrLen (Desc.Data);
    return Desc;
  }

  BORAX_SYMBOL *
  Keyword (
    IN const wchar_t  *Name
    )
  {
    BORAX_OBJECT        Condition;
    BORAX_CONST_STRING  Desc = String (Name);
    BORAX_SYMBOL        *Symbol;

    Condition = BoraxPrimitiveKeyword (Interp.get (), Desc, &Symbol);
    if (BORAX_BOOL (Condition)) {
      throw ConditionError { Interp->Alloc, Condition };
    }

    return Symbol;
  }

  std::vector<BORAX_OBJECT>
  CallLisp (
    IN BORAX_OBJECT               Function,
    IN std::vector<BORAX_OBJECT>  Args,
    IN std::optional<UINTN>       SoftLimit = std::nullopt
    )
  {
    EFI_STATUS             Status;
    BORAX_OBJECT           Condition;
    BORAX_MULTIPLE_VALUES  *VR;
    BORAX_TASK             *RawTask;
    AutoTask               Task;
    BORAX_PIN              *IORequests;

    Status = BoraxMakeMultipleValues (Interp.get (), Args.size (), &VR);
    if (EFI_ERROR (Status)) {
      throw ConditionError {
              Interp->Alloc,
              BoraxPrimitiveHeapExhausted (Interp.get ())
      };
    }

    for (UINTN I = 0; I < Args.size (); ++I) {
      VR->Values[I] = Args.begin ()[I];
    }

    Status = BoraxInterpreterSpawn (
               Interp.get (),
               NULL,      // Completion
               BORAX_NIL, // ErrorHandler
               Function,
               BORAX_MAKE_POINTER (VR),
               &RawTask
               );
    if (EFI_ERROR (Status)) {
      Condition = BoraxPrimitiveSimpleCondition (
                    Interp.get (),
                    Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_ERROR],
                    BoraxCString ((CONST CHAR16 *)L"Failed to spawn task"),
                    0,
                    NULL
                    );
      throw ConditionError { Interp->Alloc, Condition };
    }

    Task = AutoTask { RawTask, TaskDeleter () };

    if (SoftLimit) {
      Task->Stack.SoftLimit = *SoftLimit;
    }

    BoraxInterpreterRun (Interp.get (), &IORequests);

    switch (Task->State) {
      case BORAX_TASK_RETURNED:
        VR = Task->Registers.VR;
        return { VR->Values, VR->Values + VR->Length };

      case BORAX_TASK_ABORTED:
        throw TaskAbortedError { Interp->Alloc, Task->AbortCondition };

      case BORAX_TASK_DOUBLE_FAULTED:
        throw TaskDoubleFaultedError {
                Interp->Alloc,
                Task->DoubleFault.Condition1,
                Task->DoubleFault.Condition2
        };

      default:
        throw std::logic_error { "unexpected task state" };
    }
  }
};

TEST_F (InterpreterTests, NullTest) {
  // Just make sure setup works
}

class InterpreterWithCoreTests : public InterpreterTests {
public:

  void
  SetUp (
    ) override
  {
    InterpreterTests::SetUp ();
    AddPlugin (&gPluginCore);
  }
};

TEST_F (InterpreterWithCoreTests, NullTest) {
  // Just make sure setup works
}

TEST_F (InterpreterWithCoreTests, CallTest) {
  BORAX_SYMBOL                *Plus  = Intern (L"COMMON-LISP", L"+");
  std::vector <BORAX_OBJECT>  Args   = { BORAX_MAKE_FIXNUM (2), BORAX_MAKE_FIXNUM (2) };
  std::vector <BORAX_OBJECT>  Result = CallLisp (Plus->Function, Args);

  ASSERT_EQ (1u, Result.size ());
  ASSERT_EQ (BORAX_MAKE_FIXNUM (4), Result[0]);
}

TEST_F (InterpreterWithCoreTests, UndefinedFunctionTest) {
  BORAX_OBJECT  Dne = BORAX_MAKE_POINTER (
                        Intern (L"BORAX-RUNTIME", L"DOES-NOT-EXIST")
                        );

  // TODO: Check the condition data
  ASSERT_THROW (CallLisp (Dne, { }), TaskAbortedError);
}

TEST_F (InterpreterWithCoreTests, SumListTest) {
  BORAX_SYMBOL  *Numbers = Intern (L"BORAX-RUNTIME", L"NUMBERS");
  BORAX_SYMBOL  *SumList = Intern (L"BORAX-RUNTIME", L"SUM-LIST");

  std::vector <BORAX_OBJECT>  Result =
    CallLisp (BORAX_MAKE_POINTER (SumList), { Numbers->Value });

  ASSERT_EQ (1u, Result.size ());
  ASSERT_EQ (BORAX_MAKE_FIXNUM (8675607), Result[0]);
}

TEST_F (InterpreterWithCoreTests, RecursionTest) {
  BORAX_SYMBOL  *RecTest = Intern (L"BORAX-RUNTIME", L"RECURSION-TEST");

  std::vector <BORAX_OBJECT>  Args = {
    BORAX_MAKE_FIXNUM (10),
    BORAX_NIL,
  };

  std::vector <BORAX_OBJECT>  Result = CallLisp (RecTest->Function, Args);

  ASSERT_EQ (1u, Result.size ());
  ASSERT_EQ (BORAX_MAKE_FIXNUM (0), Result[0]);
}

TEST_F (InterpreterWithCoreTests, StackOverflowTest) {
  BORAX_SYMBOL  *RecTest = Intern (L"BORAX-RUNTIME", L"RECURSION-TEST");

  std::vector <BORAX_OBJECT>  Args = {
    BORAX_MAKE_FIXNUM (10),
    BORAX_NIL,
  };

  // TODO: Check the condition data
  ASSERT_THROW (
    CallLisp (RecTest->Function, Args, 50),
    TaskAbortedError
    );
}

TEST_F (InterpreterWithCoreTests, TailCallTest) {
  // TODO: Real T
  BORAX_SYMBOL  *RecTest = Intern (L"BORAX-RUNTIME", L"RECURSION-TEST");
  BORAX_SYMBOL  *T       = Intern (L"COMMON-LISP", L"T");

  std::vector <BORAX_OBJECT>  Args = {
    BORAX_MAKE_FIXNUM (10),
    BORAX_MAKE_POINTER (T),
  };

  std::vector <BORAX_OBJECT>  Result = CallLisp (RecTest->Function, Args, 50);

  ASSERT_EQ (1u, Result.size ());
  ASSERT_EQ (BORAX_MAKE_FIXNUM (0), Result[0]);
}

struct ExitTestCase {
  unsigned int     Index;
  const wchar_t    *Tag;
  const wchar_t    *Value;
};

struct WStringWrapper {
  // It would be nice to use wstring_view but the way edk2 is overriding wchar_t
  // with -fshort-wchar while linking to the system libstdc++ means that
  // everything to do with wchar_t is horribly, horribly broken
  const wchar_t    *WStr;

  explicit WStringWrapper (
                           const wchar_t  *WStr
                           ) : WStr (WStr)
  {
  }
};

std::ostream &
operator<< (
  std::ostream          &os,
  const WStringWrapper  &Wrapper
  )
{
  os << "L\"";

  for (const wchar_t *p = Wrapper.WStr; *p != L'\0'; ++p ) {
    os << static_cast<char> (*p);
  }

  return os << "\"";
}

std::ostream &
operator<< (
  std::ostream        &os,
  const ExitTestCase  &TestCase
  )
{
  return os << "ExitTestCase { " << TestCase.Index << ", "
         << WStringWrapper (TestCase.Tag) << ", "
         << WStringWrapper (TestCase.Value) << " }";
}

class InterpreterExitTests : public InterpreterWithCoreTests,
  public testing::WithParamInterface<ExitTestCase>
{
};

TEST_P (InterpreterExitTests, ExitValues) {
  ExitTestCase  Case = GetParam ();
  BORAX_SYMBOL  *F   = Intern (L"BORAX-RUNTIME", L"EXIT-TEST-CALLER");

  std::vector <BORAX_OBJECT>  Args   = { BORAX_MAKE_FIXNUM (Case.Index) };
  std::vector <BORAX_OBJECT>  Result = CallLisp (F->Function, Args);

  BORAX_OBJECT  Tag   = BORAX_MAKE_POINTER (Keyword (Case.Tag));
  BORAX_OBJECT  Value = BORAX_MAKE_POINTER (Keyword (Case.Value));

  ASSERT_EQ (2u, Result.size ());
  ASSERT_EQ (Tag, Result[0]);
  ASSERT_EQ (Value, Result[1]);
}

static const ExitTestCase  ExitTestCases[] = {
  { 0, L"APPLE",  L"AARDVARK"   },
  { 1, L"BANANA", L"BILLY-GOAT" },
  { 2, L"CHERRY", L"CARIBOU"    },
  { 3, L"OOPS",   L"OCELOT"     },
};

INSTANTIATE_TEST_SUITE_P (
  ExitTests,
  InterpreterExitTests,
  testing::ValuesIn (ExitTestCases)
  );

TEST_F (InterpreterWithCoreTests, ExpiredExitTest) {
  BORAX_SYMBOL  *Test = Intern (L"BORAX-RUNTIME", L"EXPIRED-EXIT-TEST-CALLER");

  // TODO: Check the condition data
  ASSERT_THROW (CallLisp (Test->Function, { }), TaskAbortedError);
}
