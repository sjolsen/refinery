#include "Image.h"

#include <Library/BoraxInterpreter.h>
#include <Library/BoraxMemory.h>
#include <Library/BoraxObjectFile.h>
#include <Library/BoraxSystemAllocator.h>
#include <Library/BundledResource.h>
#include <Library/DevicePathLib.h>
#include <Library/MemoryAllocationLib.h>
#include <Library/UefiLib.h>

STATIC BORAX_ALLOCATOR    gAlloc;
STATIC BORAX_INTERPRETER  gInterp;

typedef struct {
  BORAX_GLOBAL_ENVIRONMENT    Env;
  BORAX_OBJECT                Globals;
  BORAX_OBJECT                Classes;
  BORAX_OBJECT                Packages;
  BORAX_OBJECT                Numbers;
  BORAX_OBJECT                Stuff;
  BORAX_OBJECT                Vector;
  BORAX_OBJECT                Hello;
  BORAX_OBJECT                SumList;
} ROOT;

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Nil;
  BORAX_OBJECT    CommonLisp;
  BORAX_OBJECT    Keyword;
} GLOBALS;

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    StandardClass;
  BORAX_OBJECT    Package;
  BORAX_OBJECT    Symbol;
  BORAX_OBJECT    SimpleVector;
  BORAX_OBJECT    String;
  BORAX_OBJECT    BytecodeFunction;
} CLASSES;

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Name;
} STANDARD_CLASS;

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Name;
} PACKAGE;

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Package;
  BORAX_OBJECT    Name;
} SYMBOL;

typedef struct {
  BUFFER     *Content;
  GLOBALS    *Globals;
  CLASSES    *Classes;
} LISP_CONTEXT;

// TODO: Make this a function constant
STATIC LISP_CONTEXT  gCtx;

STATIC BORAX_OBJECT
EFIAPI
Car (
  BORAX_OBJECT  Object
  )
{
  BORAX_CONS  *Cons;

  if (BORAX_DISCRIMINATE (Object) != BORAX_DISCRIM_CONS) {
    return gCtx.Globals->Nil;
  }

  Cons = (BORAX_CONS *)BORAX_GET_POINTER (Object);
  return Cons->Car;
}

STATIC BORAX_OBJECT
EFIAPI
Cdr (
  BORAX_OBJECT  Object
  )
{
  BORAX_CONS  *Cons;

  if (BORAX_DISCRIMINATE (Object) != BORAX_DISCRIM_CONS) {
    return gCtx.Globals->Nil;
  }

  Cons = (BORAX_CONS *)BORAX_GET_POINTER (Object);
  return Cons->Cdr;
}

STATIC BORAX_OBJECT
EFIAPI
GetClassName (
  IN BORAX_OBJECT  Object
  )
{
  EFI_STATUS      Status;
  STANDARD_CLASS  *Class;

  Status = BORAX_GET_OBJECT_RECORD (Object, &Class);
  if (EFI_ERROR (Status)) {
    return gCtx.Globals->Nil;
  }

  if (Class->Record.Class != gCtx.Classes->StandardClass) {
    return gCtx.Globals->Nil;
  }

  return Class->Name;
}

STATIC BORAX_OBJECT
EFIAPI
GetPackageName (
  IN BORAX_OBJECT  Object
  )
{
  EFI_STATUS  Status;
  PACKAGE     *Package;

  Status = BORAX_GET_OBJECT_RECORD (Object, &Package);
  if (EFI_ERROR (Status)) {
    return gCtx.Globals->Nil;
  }

  if (Package->Record.Class != gCtx.Classes->Package) {
    return gCtx.Globals->Nil;
  }

  return Package->Name;
}

STATIC EFI_STATUS
EFIAPI
WriteString (
  IN BORAX_OBJECT  Object
  )
{
  BORAX_RECORD  *Record;
  UINTN         Length;

  if (BORAX_DISCRIMINATE (Object) != BORAX_DISCRIM_WORD_RECORD) {
    (VOID)BufferWrite (gCtx.Content, L"Not a string\n");
    return EFI_INVALID_PARAMETER;
  }

  Record = (BORAX_RECORD *)BORAX_GET_POINTER (Object);
  if (Record->Class != gCtx.Classes->String) {
    (VOID)BufferWrite (gCtx.Content, L"Not a string\n");
    return EFI_INVALID_PARAMETER;
  }

  Length = (Record->Length * sizeof (UINTN)) / sizeof (CHAR16) - Record->LengthAux;

  // TODO: Non-printable characters
  return BufferWriteChars (gCtx.Content, (CHAR16 *)Record->Data, Length);
}

STATIC EFI_STATUS
EFIAPI
FormatRecursive (
  IN BORAX_OBJECT  Object
  )
{
  switch (BORAX_DISCRIMINATE (Object)) {
    case BORAX_DISCRIM_FIXNUM:
      return BufferWriteInt (gCtx.Content, BORAX_GET_FIXNUM (Object));

    case BORAX_DISCRIM_UNBOUND:
      return BufferWrite (gCtx.Content, L"<UNBOUND>");

    case BORAX_DISCRIM_CHARACTER:
    {
      EFI_STATUS  Status;

      Status = BufferWrite (gCtx.Content, L"#\\");
      if (EFI_ERROR (Status)) {
        return Status;
      }

      // TODO: Non-printable characters
      return BufferWriteChar (gCtx.Content, BORAX_GET_CHARACTER (Object));
    }

    case BORAX_DISCRIM_CONS:
    {
      EFI_STATUS    Status;
      BORAX_OBJECT  List;
      BOOLEAN       Start = TRUE;

      Status = BufferWriteChar (gCtx.Content, L'(');
      if (EFI_ERROR (Status)) {
        return Status;
      }

      for (List = Object;
           BORAX_DISCRIMINATE (List) == BORAX_DISCRIM_CONS;
           List = Cdr (List))
      {
        BORAX_OBJECT  Value = Car (List);

        if (Start) {
          Start = FALSE;
        } else {
          Status = BufferWriteChar (gCtx.Content, L' ');
          if (EFI_ERROR (Status)) {
            return Status;
          }
        }

        Status = FormatRecursive (Value);
        if (EFI_ERROR (Status)) {
          return Status;
        }
      }

      if (List != gCtx.Globals->Nil) {
        Status = BufferWrite (gCtx.Content, L" . ");
        if (EFI_ERROR (Status)) {
          return Status;
        }

        Status = FormatRecursive (List);
        if (EFI_ERROR (Status)) {
          return Status;
        }
      }

      return BufferWriteChar (gCtx.Content, L')');
    }

    case BORAX_DISCRIM_WORD_RECORD:
    {
      EFI_STATUS    Status;
      BORAX_RECORD  *Record;

      Record = (BORAX_RECORD *)BORAX_GET_POINTER (Object);

      if (Record->Class == gCtx.Classes->String) {
        Status = BufferWriteChar (gCtx.Content, L'"');
        if (EFI_ERROR (Status)) {
          return Status;
        }

        Status = WriteString (Object);
        if (EFI_ERROR (Status)) {
          return Status;
        }

        return BufferWriteChar (gCtx.Content, L'"');
      } else {
        return BufferWrite (gCtx.Content, L"<WORD-RECORD>");
      }
    }

    case BORAX_DISCRIM_OBJECT_RECORD:
    {
      EFI_STATUS    Status;
      BORAX_RECORD  *Record;

      Record = (BORAX_RECORD *)BORAX_GET_POINTER (Object);

      if (Object == gCtx.Globals->Nil) {
        return BufferWrite (gCtx.Content, L"NIL");
      } else if (Record->Class == gCtx.Classes->Symbol) {
        SYMBOL  *Symbol;

        Status = BORAX_GET_OBJECT_RECORD (Object, &Symbol);
        if (EFI_ERROR (Status)) {
          (VOID)BufferWrite (gCtx.Content, L"Malformed symbol object\n");
          return EFI_INVALID_PARAMETER;
        }

        if (Symbol->Package != gCtx.Globals->CommonLisp) {
          if (Symbol->Package != gCtx.Globals->Keyword) {
            BORAX_OBJECT  PackageName = GetPackageName (Symbol->Package);

            Status = WriteString (PackageName);
            if (EFI_ERROR (Status)) {
              return Status;
            }
          }

          Status = BufferWriteChar (gCtx.Content, L':');
          if (EFI_ERROR (Status)) {
            return Status;
          }
        }

        return WriteString (Symbol->Name);
      } else if (Record->Class == gCtx.Classes->SimpleVector) {
        UINTN    I;
        BOOLEAN  Start = TRUE;

        Status = BufferWrite (gCtx.Content, L"#(");
        if (EFI_ERROR (Status)) {
          return Status;
        }

        for (I = 0; I < Record->Length; ++I) {
          BORAX_OBJECT  Value = Record->Data[I];

          if (Start) {
            Start = FALSE;
          } else {
            Status = BufferWriteChar (gCtx.Content, L' ');
            if (EFI_ERROR (Status)) {
              return Status;
            }
          }

          Status = FormatRecursive (Value);
          if (EFI_ERROR (Status)) {
            return Status;
          }
        }

        return BufferWriteChar (gCtx.Content, L')');
      } else if (Record->Class == gCtx.Classes->StandardClass) {
        STANDARD_CLASS  *Class;

        Status = BORAX_GET_OBJECT_RECORD (Object, &Class);
        if (EFI_ERROR (Status)) {
          (VOID)BufferWrite (gCtx.Content, L"Malformed class object\n");
          return EFI_INVALID_PARAMETER;
        }

        Status = BufferWrite (gCtx.Content, L"<STANDARD-CLASS ");
        if (EFI_ERROR (Status)) {
          return Status;
        }

        Status = FormatRecursive (Class->Name);
        if (EFI_ERROR (Status)) {
          return Status;
        }

        return BufferWriteChar (gCtx.Content, L'>');
      } else {
        BORAX_OBJECT  ClassName;
        UINTN         I;

        ClassName = GetClassName (Record->Class);

        if (ClassName == gCtx.Globals->Nil) {
          Status = BufferWrite (gCtx.Content, L"<OBJECT-RECORD ");
          if (EFI_ERROR (Status)) {
            return Status;
          }

          Status = FormatRecursive (Record->Class);
          if (EFI_ERROR (Status)) {
            return Status;
          }
        } else {
          Status = BufferWriteChar (gCtx.Content, L'<');
          if (EFI_ERROR (Status)) {
            return Status;
          }

          Status = FormatRecursive (ClassName);
          if (EFI_ERROR (Status)) {
            return Status;
          }
        }

        for (I = 0; I < Record->Length; ++I) {
          BORAX_OBJECT  Value = Record->Data[I];

          Status = BufferWriteChar (gCtx.Content, L' ');
          if (EFI_ERROR (Status)) {
            return Status;
          }

          Status = BufferWriteInt (gCtx.Content, I);
          if (EFI_ERROR (Status)) {
            return Status;
          }

          Status = BufferWriteChar (gCtx.Content, L'=');
          if (EFI_ERROR (Status)) {
            return Status;
          }

          Status = FormatRecursive (Value);
          if (EFI_ERROR (Status)) {
            return Status;
          }
        }

        return BufferWriteChar (gCtx.Content, L'>');
      }
    }

    case BORAX_DISCRIM_WEAK_POINTER:
      return BufferWrite (gCtx.Content, L"<WEAK-POINTER>");

    case BORAX_DISCRIM_PIN:
      return BufferWrite (gCtx.Content, L"<PIN>");

    case BORAX_DISCRIM_MOVED:
      return BufferWrite (gCtx.Content, L"<MOVED>");

    case BORAX_DISCRIM_UNINITIALIZED:
      return BufferWrite (gCtx.Content, L"<UNINITIALIZED>");

    default:
      return BufferWrite (gCtx.Content, L"<ILLEGAL>");
  }
}

STATIC EFI_STATUS
EFIAPI
PrintLabelled (
  IN CONST CHAR16  *Label,
  IN BORAX_OBJECT  Object
  )
{
  EFI_STATUS  Status;

  Status = BufferWrite (gCtx.Content, Label);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = BufferWrite (gCtx.Content, L" = ");
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = FormatRecursive (Object);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  return BufferWriteChar (gCtx.Content, L'\n');
}

STATIC EFI_STATUS
EFIAPI
FillContent (
  IN OUT BUFFER  *Content,
  BORAX_PIN      *RootPin
  )
{
  EFI_STATUS  Status;
  ROOT        *Root;

  gCtx.Content = Content;

  Status = BORAX_GET_OBJECT_RECORD (RootPin->Object, &Root);
  if (EFI_ERROR (Status)) {
    (VOID)BufferWrite (Content, L"Malformed root object\n");
    return EFI_INVALID_PARAMETER;
  }

  Status = BORAX_GET_OBJECT_RECORD (Root->Globals, &gCtx.Globals);
  if (EFI_ERROR (Status)) {
    (VOID)BufferWrite (Content, L"Malformed globals object\n");
    return EFI_INVALID_PARAMETER;
  }

  Status = BORAX_GET_OBJECT_RECORD (Root->Classes, &gCtx.Classes);
  if (EFI_ERROR (Status)) {
    (VOID)BufferWrite (Content, L"Malformed classes object\n");
    return EFI_INVALID_PARAMETER;
  }

  BoraxInterpreterInit (&gInterp, &gAlloc, RootPin);

  (VOID)PrintLabelled (L"NUMBERS", Root->Numbers);
  (VOID)PrintLabelled (L"STUFF", Root->Stuff);
  (VOID)PrintLabelled (L"VECTOR", Root->Vector);
  (VOID)PrintLabelled (L"HELLO", Root->Hello);
  (VOID)PrintLabelled (L"SUM-LIST", Root->SumList);

  BoraxInterpreterCleanup (&gInterp);

  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
ImageLoadContent (
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

  (VOID)FillContent (Content, RootPin);

cleanup:
  if (InitialImage != NULL) {
    InitialImage->Close (InitialImage);
  }

  FreePool (InitialImagePath);
  FreePool (InitialImageDevicePath);
  BoraxAllocatorCleanup (&gAlloc);
  return Status;
}
