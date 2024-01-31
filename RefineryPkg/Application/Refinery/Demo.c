#include "Demo.h"

#include "Display.h"

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

EFI_STATUS
EFIAPI
DemoInit (
  DEMO                             *Demo,
  EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL  *TextOut
  )
{
  EFI_STATUS  Status;

  Demo->State      = DEMO_STATE_RUNNING;
  Demo->Redraw     = TRUE;
  Demo->TextOut    = TextOut;
  Demo->Background = EFI_BLUE;
  Demo->Foreground = EFI_WHITE;
  Demo->Cursor.X   = 3;
  Demo->Cursor.Y   = 1;
  Demo->Content    = LoremIpsum;

  // Assume the display mode never changes
  Status = ScreenBounds (Demo->TextOut, &Demo->ScreenBounds);
  return Status;
}

EFI_STATUS
EFIAPI
DemoRedraw (
  DEMO  *Demo
  )
{
  EFI_STATUS    Status;
  MARGINS       DrawableMargins = { 0, 0, 0, 1 };
  BOUNDING_BOX  DrawableBox, ContentBox;

  // Returns unsupported even though it works
  (VOID)Demo->TextOut->EnableCursor (Demo->TextOut, FALSE);

  if (Demo->Redraw & DEMO_REDRAW_CONTENT) {
    Status = Demo->TextOut->SetAttribute (
                              Demo->TextOut,
                              EFI_TEXT_ATTR (Demo->Foreground, Demo->Background)
                              );
    if (EFI_ERROR (Status)) {
      return Status;
    }

    Status = Demo->TextOut->ClearScreen (Demo->TextOut);
    if (EFI_ERROR (Status)) {
      return Status;
    }

    Status = ShrinkBox (&Demo->ScreenBounds, &DrawableMargins, &DrawableBox);
    if (EFI_ERROR (Status)) {
      return Status;
    }

    Status = PrintBox (Demo->TextOut, &DrawableBox, &ContentBox);
    if (EFI_ERROR (Status)) {
      return Status;
    }

    Status = PrintContent (Demo->TextOut, &ContentBox, Demo->Content);
    if (EFI_ERROR (Status)) {
      return Status;
    }

    // We moved the cursor while redrawing the content
    Demo->Redraw |= DEMO_REDRAW_CURSOR;
  }

  if (Demo->Redraw & DEMO_REDRAW_CURSOR) {
    Status = Demo->TextOut->SetCursorPosition (
                              Demo->TextOut,
                              Demo->Cursor.X,
                              Demo->Cursor.Y
                              );
    if (EFI_ERROR (Status)) {
      return Status;
    }
  }

  (VOID)Demo->TextOut->EnableCursor (Demo->TextOut, TRUE);

  Demo->Redraw = 0;
  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
DemoHandleKeyboard (
  DEMO                 *Demo,
  const EFI_INPUT_KEY  *Key
  )
{
  switch (Key->ScanCode) {
    case SCAN_UP:
      Demo->Foreground = 0xF & (Demo->Foreground + 1);
      Demo->Redraw    |= DEMO_REDRAW_CONTENT;
      break;
    case SCAN_DOWN:
      Demo->Foreground = 0xF & (Demo->Foreground - 1);
      Demo->Redraw    |= DEMO_REDRAW_CONTENT;
      break;
    case SCAN_RIGHT:
      Demo->Background = 0x7 & (Demo->Background + 1);
      Demo->Redraw    |= DEMO_REDRAW_CONTENT;
      break;
    case SCAN_LEFT:
      Demo->Background = 0x7 & (Demo->Background - 1);
      Demo->Redraw    |= DEMO_REDRAW_CONTENT;
      break;
    case SCAN_ESC:
      Demo->State = DEMO_STATE_EXIT;
      break;
  }

  switch (Key->UnicodeChar) {
    case L'Q':
    case L'q':
      Demo->State = DEMO_STATE_SHUTDOWN;
      break;
  }

  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
DemoHandlePointer (
  DEMO                *Demo,
  const BOUNDING_BOX  *PointerArea,
  const VEC2_INTN     *Pointer
  )
{
  VEC2_INTN  NewCursor;

  RebaseVector (PointerArea, Pointer, &Demo->ScreenBounds, &NewCursor);
  if (NewCursor.X != Demo->Cursor.X) {
    Demo->Cursor.X = NewCursor.X;
    Demo->Redraw  |= DEMO_REDRAW_CURSOR;
  }

  if (NewCursor.Y != Demo->Cursor.Y) {
    Demo->Cursor.Y = NewCursor.Y;
    Demo->Redraw  |= DEMO_REDRAW_CURSOR;
  }

  return EFI_SUCCESS;
}
