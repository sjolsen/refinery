#include "Buffer.h"

#include <Library/BaseLib.h>
#include <Library/BaseMemoryLib.h>
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
  return BufferWriteChars (Buffer, String, StrLen (String));
}

EFI_STATUS
EFIAPI
BufferWriteChars (
  IN BUFFER        *Buffer,
  IN CONST CHAR16  *Chars,
  IN UINTN         Length
  )
{
  UINTN  RequiredCapacity;

  // TODO: Safe arithmetic?
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

  (VOID)CopyMem (
          Buffer->Data + Buffer->Terminator,
          Chars,
          Length * sizeof (CHAR16)
          );

  Buffer->Terminator              += Length;
  Buffer->Data[Buffer->Terminator] = L'\0';
  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
BufferWriteChar (
  IN BUFFER  *Buffer,
  IN CHAR16  Char
  )
{
  return BufferWriteChars (Buffer, &Char, 1);
}

STATIC CONST CHAR16  Digits[] = L"0123456789ABCDEF";

EFI_STATUS
EFIAPI
BufferFormatInt (
  IN BUFFER  *Buffer,
  IN INTN    Value,
  IN UINTN   Base,
  IN CHAR16  PadChar,
  IN UINTN   PadTo
  )
{
  EFI_STATUS  Status;
  BOOLEAN     Negative;
  UINTN       UValue;
  UINTN       Start, End;
  UINTN       Width = 0;

  if (Base > 16) {
    return EFI_INVALID_PARAMETER;
  }

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
    Status = BufferWriteChar (Buffer, Digits[UValue % Base]);
    if (EFI_ERROR (Status)) {
      goto error;
    }

    UValue /= Base;
    ++Width;
  } while (UValue != 0);

  // padding
  while (Width < PadTo) {
    Status = BufferWriteChar (Buffer, PadChar);
    if (EFI_ERROR (Status)) {
      goto error;
    }

    ++Width;
  }

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

EFI_STATUS
EFIAPI
BufferWriteInt (
  IN BUFFER  *Buffer,
  IN INTN    Value
  )
{
  return BufferFormatInt (Buffer, Value, 10, L' ', 0);
}

EFI_STATUS
EFIAPI
BufferWriteHex (
  IN BUFFER  *Buffer,
  IN INTN    Value,
  IN UINTN   PadTo
  )
{
  return BufferFormatInt (Buffer, Value, 16, L'0', PadTo);
}
