#include "Display.h"

#include "Flow.h"

EFI_STATUS
EFIAPI
ScreenBounds (
  EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL  *Out,
  BOUNDING_BOX                     *Bounds
  )
{
  EFI_STATUS  Status;
  UINTN       Columns, Rows;

  Status = Out->QueryMode (Out, Out->Mode->Mode, &Columns, &Rows);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Bounds->XMin = 0;
  Bounds->XMax = Columns;
  Bounds->YMin = 0;
  Bounds->YMax = Rows;

  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
PrintChar (
  EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL  *Out,
  CHAR16                           Char
  )
{
  CHAR16  String[2] = { Char, L'\0' };

  return Out->OutputString (Out, String);
}

EFI_STATUS
EFIAPI
DrawBox (
  EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL  *Out,
  const BOUNDING_BOX               *Outer,
  BOUNDING_BOX                     *Inner
  )
{
  static const MARGINS  Margins = { 1, 1, 1, 1 };
  EFI_STATUS            Status;
  INTN                  X, Y;

  Status = ShrinkBox (Outer, &Margins, Inner);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  // Top row
  Status = Out->SetCursorPosition (Out, Outer->XMin, Outer->YMin);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = PrintChar (Out, BOXDRAW_DOUBLE_DOWN_RIGHT);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  for (X = Outer->XMin + 1; X < Outer->XMax - 1; ++X) {
    Status = PrintChar (Out, BOXDRAW_DOUBLE_HORIZONTAL);
    if (EFI_ERROR (Status)) {
      return Status;
    }
  }

  Status = PrintChar (Out, BOXDRAW_DOUBLE_DOWN_LEFT);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  // Middle rows
  for (Y = Outer->YMin + 1; Y < Outer->YMax - 1; ++Y) {
    Status = Out->SetCursorPosition (Out, Outer->XMin, Y);
    if (EFI_ERROR (Status)) {
      return Status;
    }

    Status = PrintChar (Out, BOXDRAW_DOUBLE_VERTICAL);
    if (EFI_ERROR (Status)) {
      return Status;
    }

    Status = Out->SetCursorPosition (Out, Outer->XMax - 1, Y);
    if (EFI_ERROR (Status)) {
      return Status;
    }

    Status = PrintChar (Out, BOXDRAW_DOUBLE_VERTICAL);
    if (EFI_ERROR (Status)) {
      return Status;
    }
  }

  // Bottom row
  Status = Out->SetCursorPosition (Out, Outer->XMin, Outer->YMax - 1);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = PrintChar (Out, BOXDRAW_DOUBLE_UP_RIGHT);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  for (X = Outer->XMin + 1; X < Outer->XMax - 1; ++X) {
    Status = PrintChar (Out, BOXDRAW_DOUBLE_HORIZONTAL);
    if (EFI_ERROR (Status)) {
      return Status;
    }
  }

  Status = PrintChar (Out, BOXDRAW_DOUBLE_UP_LEFT);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
PrintBox (
  EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL  *Out,
  const BOUNDING_BOX               *Outer,
  BOUNDING_BOX                     *Inner
  )
{
  /*
   * |<--   6 + Width(Content)  -->|
   * ┌─────────────────────────────┐ ─
   * │.╔═════════════════════════╗.│ ^
   * │.║.Content Content Content.║.│ |
   * │.║.Content Content Content.║.│ 2 + Height(Content)
   * │.║.Content Content Content.║.│ |
   * │.╚═════════════════════════╝.│ v
   * └─────────────────────────────┘ ─
   */
  static const MARGINS  MarginOuter = { 1, 1, 0, 0 };
  static const MARGINS  MarginInner = { 1, 1, 0, 0 };
  EFI_STATUS            Status;
  BOUNDING_BOX          BoxOuter, BoxInner;

  Status = ShrinkBox (Outer, &MarginOuter, &BoxOuter);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = DrawBox (Out, &BoxOuter, &BoxInner);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  return ShrinkBox (&BoxInner, &MarginInner, Inner);
}

typedef struct _PRINT_CONTENT_CTX {
  EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL    *Out;
  const BOUNDING_BOX                 *Bounds;
  UINTN                              Y;
} PRINT_CONTENT_CTX;

EFI_STATUS
EFIAPI
PrintContentCallback (
  VOID          *VCtx,
  const CHAR16  *Line,
  UINTN         Len
  )
{
  EFI_STATUS         Status;
  PRINT_CONTENT_CTX  *Ctx = (PRINT_CONTENT_CTX *)VCtx;
  UINTN              I;

  if (Ctx->Y < Ctx->Bounds->YMax) {
    Status = Ctx->Out->SetCursorPosition (Ctx->Out, Ctx->Bounds->XMin, Ctx->Y);
    if (EFI_ERROR (Status)) {
      return Status;
    }

    for (I = 0; I < Len; ++I) {
      Status = PrintChar (Ctx->Out, Line[I]);
      if (EFI_ERROR (Status)) {
        return Status;
      }
    }

    ++Ctx->Y;
  }

  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
PrintContent (
  EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL  *Out,
  const BOUNDING_BOX               *Bounds,
  const CHAR16                     *Content
  )
{
  PRINT_CONTENT_CTX  Ctx   = { Out, Bounds, Bounds->YMin };
  FLOW_CALLBACK      Cb    = { &Ctx, &PrintContentCallback };
  UINTN              Width = Bounds->XMax - Bounds->XMin;

  return FlowContent (Content, Width, &Cb);
}
