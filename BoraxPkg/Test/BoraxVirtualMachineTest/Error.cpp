#include "Error.hpp"

extern "C" {
  #include <Library/PrintLib.h>
}

namespace borax::testing {
namespace {
std::string
Format (
  IN CONST CHAR8  *FormatString,
  ...
  )
{
  VA_LIST  List1, List2;

  VA_START (List1, FormatString);
  VA_COPY (List2, List1);

  UINTN        Length = SPrintLengthAsciiFormat (FormatString, List1);
  std::string  Result (Length + 1, '\0');

  Length = AsciiVSPrint (Result.data (), Length, FormatString, List2);
  Result.resize (Length);

  VA_END (List1);
  VA_END (List2);

  return Result;
}
}  // namespace

EFIError::EFIError (
                    EFI_STATUS  Status
                    )
  : Error (Format ("%r", Status)),
  Status_ (Status)
{
}

AutoPin
ConditionError::PinCondition (
  IN BORAX_ALLOCATOR  *Alloc,
  IN BORAX_OBJECT     Condition
  )
{
  EFI_STATUS  Status;
  BORAX_PIN   *Pin;

  Status = BoraxAllocatePin (Alloc, Condition, &Pin);
  if (EFI_ERROR (Status)) {
    throw EFIError { Status };
  }

  return AutoPin (Pin, PinDeleter ());
}

ConditionError::ConditionError (
                                IN BORAX_ALLOCATOR  *Alloc,
                                IN BORAX_OBJECT     Condition
                                )
  : Error ("(Condition)"),
  Condition_ (PinCondition (Alloc, Condition))
{
}
}  // namespace borax::testing
