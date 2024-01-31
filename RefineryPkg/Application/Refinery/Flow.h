#ifndef REFINERY_FLOW_H
#define REFINERY_FLOW_H

typedef struct _FLOW_CALLBACK {
  VOID    *Ctx;
  EFI_STATUS (EFIAPI *Line)(VOID *Ctx, const CHAR16 *String, UINTN Len);
} FLOW_CALLBACK;

EFI_STATUS
EFIAPI
FlowContent (
  const CHAR16         *Content,
  UINTN                Width,
  const FLOW_CALLBACK  *Cb
  );

#endif // REFINERY_FLOW_H
