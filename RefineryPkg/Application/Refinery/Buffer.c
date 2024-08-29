#include "Buffer.h"

#include <Library/BaseLib.h>
#include <Library/MemoryAllocationLib.h>

#define INITIAL_BUFFER_SIZE  10
#define GROWTH_FACTOR        2

EFI_STATUS
EFIAPI
BufferInit (
  OUT BUFFER  *Buffer
  )
{
  Buffer->Data = AllocatePool (INITIAL_BUFFER_SIZE * sizeof (CHAR16));
  if (Buffer->Data == NULL) {
    return EFI_OUT_OF_RESOURCES;
  }

  Buffer->Data[0]    = L'\0';
  Buffer->Terminator = 0;
  Buffer->Capacity   = INITIAL_BUFFER_SIZE;

  return EFI_SUCCESS;
}

VOID
EFIAPI
BufferDestroy (
  IN BUFFER  *Buffer
  )
{
  (VOID)FreePool (Buffer->Data);
}

EFI_STATUS
EFIAPI
BufferWrite (
  IN BUFFER        *Buffer,
  IN CONST CHAR16  *String
  )
{
  EFI_STATUS  Status;
  UINTN       Length;
  UINTN       RequiredCapacity;

  // TODO: Safe arithmetic?
  Length           = StrLen (String);
  RequiredCapacity = Buffer->Terminator + Length + 1;

  if (RequiredCapacity > Buffer->Capacity) {
    CHAR16  *NewData;
    UINTN   NewCapacity;

    NewCapacity = MAX (GROWTH_FACTOR * Buffer->Capacity, RequiredCapacity);

    NewData = ReallocatePool (
                Buffer->Capacity * sizeof (CHAR16),
                NewCapacity * sizeof (CHAR16),
                Buffer->Data
                );
    if (NewData == NULL) {
      return EFI_OUT_OF_RESOURCES;
    }

    Buffer->Data     = NewData;
    Buffer->Capacity = NewCapacity;
  }

  Status = StrCpyS (
             Buffer->Data + Buffer->Terminator,
             Buffer->Capacity - Buffer->Terminator,
             String
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Buffer->Terminator += Length;
  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
BufferWriteChar (
  IN BUFFER  *Buffer,
  IN CHAR16  Char
  )
{
  CHAR16  String[2] = { Char, L'\0' };

  return BufferWrite (Buffer, String);
}

EFI_STATUS
EFIAPI
BufferWriteInt (
  IN BUFFER  *Buffer,
  IN INTN    Value
  )
{
  EFI_STATUS  Status;
  BOOLEAN     Negative;
  UINTN       UValue;
  UINTN       Start, End;

  // absolute value
  Negative = Value < 0;
  if (Negative) {
    UValue = ~((UINTN)Value) + 1;
  } else {
    UValue = (UINTN)Value;
  }

  // digits
  Start = Buffer->Terminator;
  do {
    Status = BufferWriteChar (Buffer, L'0' + (UValue % 10));
    if (EFI_ERROR (Status)) {
      goto error;
    }

    UValue /= 10;
  } while (UValue != 0);

  // sign
  if (Negative) {
    Status = BufferWriteChar (Buffer, L'-');
    if (EFI_ERROR (Status)) {
      goto error;
    }
  }

  End = Buffer->Terminator;

  // reverse
  while (Start < End - 1) {
    CHAR16  A = Buffer->Data[Start];
    CHAR16  B = Buffer->Data[End - 1];

    Buffer->Data[Start]   = B;
    Buffer->Data[End - 1] = A;
    ++Start;
    --End;
  }

  return EFI_SUCCESS;

error:
  Buffer->Terminator  = Start;
  Buffer->Data[Start] = L'\0';
  return Status;
}
