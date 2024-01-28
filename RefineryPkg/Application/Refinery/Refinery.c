#include <Uefi.h>
#include <Library/UefiBootServicesTableLib.h>
#include <Library/UefiLib.h>
#include <Library/UefiRuntimeServicesTableLib.h>

EFI_STATUS
EFIAPI
WaitForKeyStroke (
  EFI_SIMPLE_TEXT_INPUT_PROTOCOL  *In,
  EFI_INPUT_KEY                   *Key
  )
{
  EFI_STATUS  Status;
  UINTN       Index;

  Status = In->ReadKeyStroke (In, Key);
  while (Status == EFI_NOT_READY) {
    Status = gBS->WaitForEvent (1, &In->WaitForKey, &Index);
    if (EFI_ERROR (Status)) {
      return Status;
    }

    Status = In->ReadKeyStroke (In, Key);
  }

  return Status;
}

typedef struct _BOUNDING_BOX {
  UINTN    XMin, XMax;
  UINTN    YMin, YMax;
} BOUNDING_BOX;

typedef struct _MARGINS {
  UINTN    Left, Right;
  UINTN    Top, Bottom;
} MARGINS;

EFI_STATUS
EFIAPI
ShrinkBox (
  const BOUNDING_BOX  *Outer,
  const MARGINS       *Margins,
  BOUNDING_BOX        *Inner
  )
{
  if (Outer->XMin + Margins->Left + Margins->Right > Outer->XMax) {
    return EFI_INVALID_PARAMETER;
  }

  if (Outer->YMin + Margins->Top + Margins->Bottom > Outer->YMax) {
    return EFI_INVALID_PARAMETER;
  }

  Inner->XMin = Outer->XMin + Margins->Left;
  Inner->XMax = Outer->XMax - Margins->Right;
  Inner->YMin = Outer->YMin + Margins->Top;
  Inner->YMax = Outer->YMax - Margins->Bottom;
  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
ScreenBounds (
  EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL  *Out,
  BOUNDING_BOX                     *Bounds
  )
{
  Bounds->XMin = 0;
  Bounds->YMin = 0;
  return Out->QueryMode (Out, Out->Mode->Mode, &Bounds->XMax, &Bounds->YMax);
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
  UINTN                 X, Y;

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

static const CHAR16  LoremIpsum[] = L"\
Libero iste soluta quia corrupti similique ut. Aut fugiat impedit rerum omnis \
maxime quam. Modi aut tempora omnis facere error cum rem quaerat.\
\n\n\
Et temporibus aut id facilis. Repudiandae quas dolorem ipsum nobis totam aut \
ipsa est. Minima dolorum a nihil.\
\n\n\
Quod et rem magni consequatur fugit similique non ut. Expedita eveniet et sunt \
eos cum. Exercitationem vel perspiciatis voluptatem quos explicabo dolorem \
doloremque. Molestiae vel aut ad quasi animi nihil aliquam. Enim qui placeat \
ratione possimus occaecati qui nostrum consequuntur. Eos odit officiis dolore \
placeat nostrum.\
\n\n\
Non labore quod asperiores explicabo veniam consequuntur. Ut facere aut quos. \
Nulla et temporibus tempora sint magnam non. Iusto eius voluptas et \
praesentium recusandae ea.\
\n\n\
Error sed sint in. A consequatur ut voluptatem nihil ipsam id ex. Voluptatem \
neque laborum labore fugit ipsum voluptatem illo iure. Distinctio doloribus \
dolor ipsum distinctio aliquid recusandae.";

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

typedef struct _LINE_CALLBACK {
  VOID    *Ctx;
  EFI_STATUS (EFIAPI *Line)(VOID *Ctx, const CHAR16 *String, UINTN Len);
} LINE_CALLBACK;

EFI_STATUS
EFIAPI
FlowContent (
  EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL  *Out,
  const CHAR16                     *Content,
  UINTN                            Width,
  const LINE_CALLBACK              *Cb
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
  LINE_CALLBACK      Cb    = { &Ctx, &PrintContentCallback };
  UINTN              Width = Bounds->XMax - Bounds->XMin;

  return FlowContent (Out, Content, Width, &Cb);
}

EFI_STATUS
EFIAPI
Demo (
  EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL  *Out,
  UINTN                            Attribute
  )
{
  EFI_STATUS            Status;
  static const MARGINS  Margins = { 0, 0, 0, 1 };
  BOUNDING_BOX          Screen, Outer, Inner;

  Status = Out->SetAttribute (Out, Attribute);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = Out->ClearScreen (Out);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = ScreenBounds (Out, &Screen);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = ShrinkBox (&Screen, &Margins, &Outer);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = PrintBox (Out, &Outer, &Inner);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  return PrintContent (Out, &Inner, LoremIpsum);
}

EFI_STATUS
EFIAPI
RefineryMain (
  IN EFI_HANDLE        ImageHandle,
  IN EFI_SYSTEM_TABLE  *SystemTable
  )
{
  EFI_STATUS     Status;
  UINTN          Background = EFI_BLUE;
  UINTN          Foreground = EFI_WHITE;
  EFI_INPUT_KEY  Key;
  BOOLEAN        Refresh = TRUE;
  BOOLEAN        Quit    = FALSE;

  while (!Quit) {
    if (Refresh) {
      Status = Demo (gST->ConOut, EFI_TEXT_ATTR (Foreground, Background));
      if (EFI_ERROR (Status)) {
        return Status;
      }

      Refresh = FALSE;
    }

    Status = WaitForKeyStroke (gST->ConIn, &Key);
    if (EFI_ERROR (Status)) {
      return Status;
    }

    switch (Key.ScanCode) {
      case SCAN_UP:
        Foreground = 0xF & (Foreground + 1);
        Refresh    = TRUE;
        break;
      case SCAN_DOWN:
        Foreground = 0xF & (Foreground - 1);
        Refresh    = TRUE;
        break;
      case SCAN_RIGHT:
        Background = 0x7 & (Background + 1);
        Refresh    = TRUE;
        break;
      case SCAN_LEFT:
        Background = 0x7 & (Background - 1);
        Refresh    = TRUE;
        break;
      case SCAN_ESC:
        Quit = TRUE;
        break;
    }

    switch (Key.UnicodeChar) {
      case L'Q':
      case L'q':
        gRT->ResetSystem (EfiResetShutdown, EFI_SUCCESS, 0, NULL);
        break;
    }
  }

  return EFI_SUCCESS;
}
