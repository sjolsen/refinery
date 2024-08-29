#ifndef REFINERY_BUFFER_H
#define REFINERY_BUFFER_H

typedef struct {
  CHAR16    *Data;
  UINTN     Terminator;
  UINTN     Capacity;
} BUFFER;

EFI_STATUS
EFIAPI
BufferInit (
  OUT BUFFER  *Buffer
  );

VOID
EFIAPI
BufferDestroy (
  IN BUFFER  *Buffer
  );

EFI_STATUS
EFIAPI
BufferWrite (
  IN BUFFER        *Buffer,
  IN CONST CHAR16  *String
  );

EFI_STATUS
EFIAPI
BufferWriteChar (
  IN BUFFER  *Buffer,
  IN CHAR16  Char
  );

EFI_STATUS
EFIAPI
BufferWriteInt (
  IN BUFFER  *Buffer,
  IN INTN    Value
  );

#endif // REFINERY_BUFFER_H
