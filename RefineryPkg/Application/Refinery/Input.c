#include <Library/UefiBootServicesTableLib.h>

#include "Input.h"

EFI_STATUS
EFIAPI
InputInit (
  INPUT_STATE                     *Input,
  DEMO                            *Demo,
  EFI_SIMPLE_TEXT_INPUT_PROTOCOL  *TextIn,
  EFI_SIMPLE_POINTER_PROTOCOL     *Pointer
  )
{
  EFI_SIMPLE_POINTER_MODE  *Mode;

  Input->Demo    = Demo;
  Input->TextIn  = TextIn;
  Input->Pointer = Pointer;

  Mode = Input->Pointer->Mode;
  if ((Mode->ResolutionX == 0) || (Mode->ResolutionY == 0)) {
    return EFI_INVALID_PARAMETER;
  }

  // Resolution is specified in count/mm and the pointer is specified to move 2%
  // of the screen width/height per millimenter
  Input->PointerArea.XMin = 0;
  Input->PointerArea.XMax = (50 * Mode->ResolutionX);
  Input->PointerArea.YMin = 0;
  Input->PointerArea.YMax = (50 * Mode->ResolutionY);
  RebaseVector (
    &Demo->ScreenBounds,
    &Demo->Cursor,
    &Input->PointerArea,
    &Input->PointerLocation
    );

  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
InputDispatch (
  INPUT_STATE  *Input
  )
{
  EFI_STATUS                Status;
  EFI_EVENT                 Events[] = {
    Input->TextIn->WaitForKey,
    Input->Pointer->WaitForInput
  };
  UINTN                     Index;
  EFI_INPUT_KEY             Key;
  EFI_SIMPLE_POINTER_STATE  PointerState;

  Status = gBS->WaitForEvent (2, Events, &Index);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  switch (Index) {
    case 0:
      Status = Input->TextIn->ReadKeyStroke (Input->TextIn, &Key);
      if (EFI_ERROR (Status)) {
        return Status;
      }

      Status = DemoHandleKeyboard (Input->Demo, &Key);
      if (EFI_ERROR (Status)) {
        return Status;
      }

      break;
    case 1:
      Status = Input->Pointer->GetState (Input->Pointer, &PointerState);
      if (EFI_ERROR (Status)) {
        return Status;
      }

      Input->PointerLocation.X += PointerState.RelativeMovementX;
      Input->PointerLocation.Y += PointerState.RelativeMovementY;
      ClampVector (&Input->PointerArea, &Input->PointerLocation);

      Status = DemoHandlePointer (
                 Input->Demo,
                 &Input->PointerArea,
                 &Input->PointerLocation
                 );
      if (EFI_ERROR (Status)) {
        return Status;
      }

      break;

    default:
      return EFI_INVALID_PARAMETER;
  }

  return EFI_SUCCESS;
}
