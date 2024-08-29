#include "Demo.h"

#include "Display.h"
#include "Library/BoraxMemory.h"
#include "Library/BoraxObjectFile.h"
#include "Library/BoraxSystemAllocator.h"
#include "Library/BundledResource.h"
#include "Library/DevicePathLib.h"
#include "Library/MemoryAllocationLib.h"
#include "Library/UefiLib.h"

STATIC BORAX_ALLOCATOR  gAlloc;

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Nil;
  BORAX_OBJECT    Numbers;
} ROOT;

STATIC BORAX_OBJECT
EFIAPI
Car (
  ROOT          *Root,
  BORAX_OBJECT  Object
  )
{
  BORAX_CONS  *Cons;

  if (BORAX_DISCRIMINATE (Object) != BORAX_DISCRIM_CONS) {
    return Root->Nil;
  }

  Cons = (BORAX_CONS *)BORAX_GET_POINTER (Object);
  return Cons->Car;
}

STATIC BORAX_OBJECT
EFIAPI
Cdr (
  ROOT          *Root,
  BORAX_OBJECT  Object
  )
{
  BORAX_CONS  *Cons;

  if (BORAX_DISCRIMINATE (Object) != BORAX_DISCRIM_CONS) {
    return Root->Nil;
  }

  Cons = (BORAX_CONS *)BORAX_GET_POINTER (Object);
  return Cons->Cdr;
}

STATIC EFI_STATUS
EFIAPI
DemoFillContent (
  IN OUT BUFFER    *Content,
  IN BORAX_OBJECT  RootObject
  )
{
  ROOT          *Root;
  BORAX_OBJECT  List;

  if (BORAX_DISCRIMINATE (RootObject) != BORAX_DISCRIM_OBJECT_RECORD) {
    (VOID)BufferWrite (Content, L"Not an object record\n");
    return EFI_INVALID_PARAMETER;
  }

  Root = (ROOT *)BORAX_GET_POINTER (RootObject);
  if (Root->Record.Length < BORAX_RECORD_LENGTH (ROOT)) {
    (VOID)BufferWrite (Content, L"Record not large enough\n");
    return EFI_INVALID_PARAMETER;
  }

  for (List = Root->Numbers; List != Root->Nil; List = Cdr (Root, List)) {
    BORAX_OBJECT  Object = Car (Root, List);
    UINTN         Value;

    if (!BORAX_IS_FIXNUM (Object)) {
      (VOID)BufferWrite (Content, L"Not a fixnum\n");
      return EFI_INVALID_PARAMETER;
    }

    Value = BORAX_GET_FIXNUM (Object);
    (VOID)BufferWriteInt (Content, Value);
    (VOID)BufferWriteChar (Content, L'\n');
  }

  return EFI_SUCCESS;
}

STATIC EFI_STATUS
EFIAPI
DemoLoadInitialImage (
  IN OUT BUFFER  *Content
  )
{
  EFI_STATUS                Status;
  EFI_DEVICE_PATH_PROTOCOL  *InitialImageDevicePath = NULL;
  EFI_DEVICE_PATH_PROTOCOL  *Remainder;
  CHAR16                    *InitialImagePath = NULL;
  EFI_FILE_PROTOCOL         *InitialImage     = NULL;
  BORAX_PIN                 *RootPin;

  BoraxAllocatorInit (&gAlloc, &gSystemAllocator);

  Status = BundledResourcePath (L"initial-image.bxo", &InitialImageDevicePath);
  if (EFI_ERROR (Status)) {
    (VOID)BufferWrite (Content, L"Failed to construct device path\n");
    goto cleanup;
  }

  InitialImagePath = ConvertDevicePathToText (
                       InitialImageDevicePath,
                       TRUE,
                       TRUE
                       );
  if (InitialImagePath == NULL) {
    (VOID)BufferWrite (Content, L"Failed to render device path\n");
    Status = EFI_OUT_OF_RESOURCES;
    goto cleanup;
  }

  Remainder = InitialImageDevicePath;
  Status    = EfiOpenFileByDevicePath (
                &Remainder,
                &InitialImage,
                EFI_FILE_MODE_READ,
                0
                );
  if (EFI_ERROR (Status)) {
    (VOID)BufferWrite (Content, L"Failed to open ");
    (VOID)BufferWrite (Content, InitialImagePath);
    (VOID)BufferWrite (Content, L"\n");
    goto cleanup;
  }

  Status = BoraxLoadObjectFile (&gAlloc, InitialImage, &RootPin);
  if (EFI_ERROR (Status)) {
    (VOID)BufferWrite (Content, L"Failed to load ");
    (VOID)BufferWrite (Content, InitialImagePath);
    (VOID)BufferWrite (Content, L"\n");
    goto cleanup;
  }

  (VOID)BufferWrite (Content, L"Loaded ");
  (VOID)BufferWrite (Content, InitialImagePath);
  (VOID)BufferWrite (Content, L"\n");

  (VOID)DemoFillContent (Content, RootPin->Object);

cleanup:
  if (InitialImage != NULL) {
    InitialImage->Close (InitialImage);
  }

  FreePool (InitialImagePath);
  FreePool (InitialImageDevicePath);
  BoraxAllocatorCleanup (&gAlloc);
  return Status;
}

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

  // Assume the display mode never changes
  Status = ScreenBounds (Demo->TextOut, &Demo->ScreenBounds);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = BufferInit (&Demo->Content);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = DemoLoadInitialImage (&Demo->Content);
  if (EFI_ERROR (Status)) {
    BufferDestroy (&Demo->Content);
    return Status;
  }

  return EFI_SUCCESS;
}

VOID
EFIAPI
DemoCleanup (
  IN DEMO  *Demo
  )
{
  BufferDestroy (&Demo->Content);
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

    Status = PrintContent (Demo->TextOut, &ContentBox, Demo->Content.Data);
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
