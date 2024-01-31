#include "Flow.h"

UINTN
EFIAPI
WordLength (
  const CHAR16  *String
  )
{
  UINTN         Len = 0;
  const CHAR16  *P  = String;

  while (*P != L'\0' && *P != L' ' && *P != L'\n') {
    ++Len;
    ++P;
  }

  return Len;
}

EFI_STATUS
EFIAPI
FlowContent (
  const CHAR16         *Content,
  UINTN                Width,
  const FLOW_CALLBACK  *Cb
  )
{
  EFI_STATUS    Status;
  const CHAR16  *Next;
  CHAR16        C;
  const CHAR16  *Line;
  UINTN         Len;

  Next = Content;
  Line = Next;
  Len  = 0;
  while ((C = *Next++) != L'\0') {
    BOOLEAN  LineBreak = FALSE;
    switch (C) {
      case L' ':
        if (Len == 0) {
          Line = Next;
        } else {
          UINTN  WordLen = WordLength (Next);
          if (Len + 1 + WordLen > Width) {
            LineBreak = TRUE;
          } else {
            Len  += 1 + WordLen;
            Next += WordLen;
          }
        }

        break;
      case L'\n':
        LineBreak = TRUE;
        break;
      default:
        ++Len;
        break;
    }

    if (LineBreak) {
      Status = Cb->Line (Cb->Ctx, Line, Len);
      if (EFI_ERROR (Status)) {
        return Status;
      }

      Line = Next;
      Len  = 0;
    }
  }

  if (Len > 0) {
    return Cb->Line (Cb->Ctx, Line, Len);
  } else {
    return EFI_SUCCESS;
  }
}
