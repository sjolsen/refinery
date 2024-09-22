#include "MemoryTest.hpp"

extern "C" {
  #include <Library/BoraxInterpreter.h>
  #include <Library/BoraxObjectFile.h>
  #include <Library/BoraxPlugin.h>
  #include <Library/BoraxRuntime.h>
}

#include "BoraxVirtualMachineTest.hpp"
#include "MockEvent.hpp"
#include "MockFile.hpp"

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

class InterpreterInitError : public std::exception {
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
      throw InterpreterInitError { };
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
      throw InterpreterInitError { };
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
};

TEST_F (InterpreterTests, NullTest) {
  // Just make sure setup works
}

TEST_F (InterpreterTests, AddCoreTest) {
  BORAX_OBJECT  Condition;

  Condition = BoraxAddPlugin (Interp.get (), &gPluginCore, nullptr);
  // TODO: Maybe add a predicate for null conditions
  ASSERT_FALSE (BORAX_BOOL (Condition));
}
