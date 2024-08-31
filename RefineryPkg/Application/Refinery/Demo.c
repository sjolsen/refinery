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
  BORAX_OBJECT    Globals;
  BORAX_OBJECT    Classes;
  BORAX_OBJECT    Numbers;
  BORAX_OBJECT    Stuff;
  BORAX_OBJECT    Vector;
  BORAX_OBJECT    Hello;
} ROOT;

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Nil;
} GLOBALS;

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    StandardClass;
  BORAX_OBJECT    Package;
  BORAX_OBJECT    Symbol;
  BORAX_OBJECT    String;
} CLASSES;

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Name;
} STANDARD_CLASS;

typedef struct {
  BUFFER     *Content;
  GLOBALS    *Globals;
  CLASSES    *Classes;
} LISP_CONTEXT;

STATIC BORAX_OBJECT
EFIAPI
GetRecord (
  IN LISP_CONTEXT   *Ctx,
  IN BORAX_OBJECT   Object,
  IN UINTN          MinLength,
  OUT BORAX_RECORD  **Record
  )
{
  BORAX_RECORD  *TheRecord;

  if (BORAX_DISCRIMINATE (Object) != BORAX_DISCRIM_OBJECT_RECORD) {
    (VOID)BufferWrite (Ctx->Content, L"Not an object record\n");
    return EFI_INVALID_PARAMETER;
  }

  TheRecord = (BORAX_RECORD *)BORAX_GET_POINTER (Object);
  if (TheRecord->Length < MinLength) {
    (VOID)BufferWrite (Ctx->Content, L"Record too small (");
    (VOID)BufferWriteInt (Ctx->Content, TheRecord->Length);
    (VOID)BufferWrite (Ctx->Content, L" < ");
    (VOID)BufferWriteInt (Ctx->Content, MinLength);
    (VOID)BufferWrite (Ctx->Content, L")\n");
    return EFI_INVALID_PARAMETER;
  }

  *Record = TheRecord;
  return EFI_SUCCESS;
}

#define GET_RECORD(_ctx, _obj, _ptr) \
(GetRecord ((_ctx), (_obj), BORAX_RECORD_LENGTH (**(_ptr)), (BORAX_RECORD **)(_ptr)))

STATIC BORAX_OBJECT
EFIAPI
Car (
  LISP_CONTEXT  *Ctx,
  BORAX_OBJECT  Object
  )
{
  BORAX_CONS  *Cons;

  if (BORAX_DISCRIMINATE (Object) != BORAX_DISCRIM_CONS) {
    return Ctx->Globals->Nil;
  }

  Cons = (BORAX_CONS *)BORAX_GET_POINTER (Object);
  return Cons->Car;
}

STATIC BORAX_OBJECT
EFIAPI
Cdr (
  LISP_CONTEXT  *Ctx,
  BORAX_OBJECT  Object
  )
{
  BORAX_CONS  *Cons;

  if (BORAX_DISCRIMINATE (Object) != BORAX_DISCRIM_CONS) {
    return Ctx->Globals->Nil;
  }

  Cons = (BORAX_CONS *)BORAX_GET_POINTER (Object);
  return Cons->Cdr;
}

STATIC EFI_STATUS
EFIAPI
FormatRecursive (
  LISP_CONTEXT     *Ctx,
  IN BORAX_OBJECT  Object
  )
{
  switch (BORAX_DISCRIMINATE (Object)) {
    case BORAX_DISCRIM_FIXNUM:
      return BufferWriteInt (Ctx->Content, BORAX_GET_FIXNUM (Object));

    case BORAX_DISCRIM_UNBOUND:
      return BufferWrite (Ctx->Content, L"<UNBOUND>");

    case BORAX_DISCRIM_CHARACTER:
    {
      EFI_STATUS  Status;

      Status = BufferWrite (Ctx->Content, L"#\\");
      if (EFI_ERROR (Status)) {
        return Status;
      }

      // TODO: Non-printable characters
      return BufferWriteChar (Ctx->Content, BORAX_GET_CHARACTER (Object));
    }

    case BORAX_DISCRIM_CONS:
    {
      EFI_STATUS    Status;
      BORAX_OBJECT  List;
      BOOLEAN       Start = TRUE;

      Status = BufferWriteChar (Ctx->Content, L'(');
      if (EFI_ERROR (Status)) {
        return Status;
      }

      for (List = Object;
           BORAX_DISCRIMINATE (List) == BORAX_DISCRIM_CONS;
           List = Cdr (Ctx, List))
      {
        BORAX_OBJECT  Value = Car (Ctx, List);

        if (Start) {
          Start = FALSE;
        } else {
          Status = BufferWriteChar (Ctx->Content, L' ');
          if (EFI_ERROR (Status)) {
            return Status;
          }
        }

        Status = FormatRecursive (Ctx, Value);
        if (EFI_ERROR (Status)) {
          return Status;
        }
      }

      if (List != Ctx->Globals->Nil) {
        Status = BufferWrite (Ctx->Content, L" . ");
        if (EFI_ERROR (Status)) {
          return Status;
        }

        Status = FormatRecursive (Ctx, List);
        if (EFI_ERROR (Status)) {
          return Status;
        }
      }

      return BufferWriteChar (Ctx->Content, L')');
    }

    case BORAX_DISCRIM_WORD_RECORD:
    {
      EFI_STATUS    Status;
      BORAX_RECORD  *Record;

      Record = (BORAX_RECORD *)BORAX_GET_POINTER (Object);

      if (Record->Class == Ctx->Classes->String) {
        UINTN  Length;

        Length = (Record->Length * sizeof (UINTN)) / sizeof (CHAR16) - Record->LengthAux;

        Status = BufferWriteChar (Ctx->Content, L'"');
        if (EFI_ERROR (Status)) {
          return Status;
        }

        // TODO: Non-printable characters
        Status = BufferWriteChars (Ctx->Content, (CHAR16 *)Record->Data, Length);
        if (EFI_ERROR (Status)) {
          return Status;
        }

        return BufferWriteChar (Ctx->Content, L'"');
      } else {
        return BufferWrite (Ctx->Content, L"<WORD-RECORD>");
      }
    }

    case BORAX_DISCRIM_OBJECT_RECORD:
    {
      EFI_STATUS    Status;
      BORAX_RECORD  *Record;

      Record = (BORAX_RECORD *)BORAX_GET_POINTER (Object);

      if (Object == Ctx->Globals->Nil) {
        return BufferWrite (Ctx->Content, L"NIL");
      } else if (Record->Class == Ctx->Classes->StandardClass) {
        STANDARD_CLASS  *Class;

        Status = GET_RECORD (Ctx, Object, &Class);
        if (EFI_ERROR (Status)) {
          (VOID)BufferWrite (Ctx->Content, L"Malformed class object\n");
          return EFI_INVALID_PARAMETER;
        }

        Status = BufferWrite (Ctx->Content, L"<STANDARD-CLASS ");
        if (EFI_ERROR (Status)) {
          return Status;
        }

        Status = FormatRecursive (Ctx, Class->Name);
        if (EFI_ERROR (Status)) {
          return Status;
        }

        return BufferWriteChar (Ctx->Content, L'>');
      } else {
        UINTN    I;
        BOOLEAN  Start = TRUE;

        Status = BufferWrite (Ctx->Content, L"<OBJECT-RECORD ");
        if (EFI_ERROR (Status)) {
          return Status;
        }

        Status = FormatRecursive (Ctx, Record->Class);
        if (EFI_ERROR (Status)) {
          return Status;
        }

        Status = BufferWrite (Ctx->Content, L" #(");
        if (EFI_ERROR (Status)) {
          return Status;
        }

        for (I = 0; I < Record->Length; ++I) {
          BORAX_OBJECT  Value = Record->Data[I];

          if (Start) {
            Start = FALSE;
          } else {
            Status = BufferWriteChar (Ctx->Content, L' ');
            if (EFI_ERROR (Status)) {
              return Status;
            }
          }

          Status = FormatRecursive (Ctx, Value);
          if (EFI_ERROR (Status)) {
            return Status;
          }
        }

        return BufferWrite (Ctx->Content, L")>");
      }
    }

    case BORAX_DISCRIM_WEAK_POINTER:
      return BufferWrite (Ctx->Content, L"<WEAK-POINTER>");

    case BORAX_DISCRIM_PIN:
      return BufferWrite (Ctx->Content, L"<PIN>");

    case BORAX_DISCRIM_MOVED:
      return BufferWrite (Ctx->Content, L"<MOVED>");

    case BORAX_DISCRIM_UNINITIALIZED:
      return BufferWrite (Ctx->Content, L"<UNINITIALIZED>");

    default:
      return BufferWrite (Ctx->Content, L"<ILLEGAL>");
  }
}

STATIC EFI_STATUS
EFIAPI
PrintLabelled (
  IN LISP_CONTEXT  *Ctx,
  IN CONST CHAR16  *Label,
  IN BORAX_OBJECT  Object
  )
{
  EFI_STATUS  Status;

  Status = BufferWrite (Ctx->Content, Label);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = BufferWrite (Ctx->Content, L" = ");
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = FormatRecursive (Ctx, Object);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  return BufferWriteChar (Ctx->Content, L'\n');
}

STATIC EFI_STATUS
EFIAPI
DemoFillContent (
  IN OUT BUFFER    *Content,
  IN BORAX_OBJECT  RootObject
  )
{
  EFI_STATUS    Status;
  LISP_CONTEXT  Ctx;
  ROOT          *Root;

  Ctx.Content = Content;

  Status = GET_RECORD (&Ctx, RootObject, &Root);
  if (EFI_ERROR (Status)) {
    (VOID)BufferWrite (Content, L"Malformed root object\n");
    return EFI_INVALID_PARAMETER;
  }

  Status = GET_RECORD (&Ctx, Root->Globals, &Ctx.Globals);
  if (EFI_ERROR (Status)) {
    (VOID)BufferWrite (Content, L"Malformed globals object\n");
    return EFI_INVALID_PARAMETER;
  }

  Status = GET_RECORD (&Ctx, Root->Classes, &Ctx.Classes);
  if (EFI_ERROR (Status)) {
    (VOID)BufferWrite (Content, L"Malformed classes object\n");
    return EFI_INVALID_PARAMETER;
  }

  (VOID)PrintLabelled (&Ctx, L"GLOBALS", Root->Globals);
  (VOID)PrintLabelled (&Ctx, L"CLASSES", Root->Classes);
  (VOID)PrintLabelled (&Ctx, L"NUMBERS", Root->Numbers);
  (VOID)PrintLabelled (&Ctx, L"STUFF", Root->Stuff);
  (VOID)PrintLabelled (&Ctx, L"VECTOR", Root->Vector);
  (VOID)PrintLabelled (&Ctx, L"HELLO", Root->Hello);

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
