#ifndef REFINERY_INPUT_H
#define REFINERY_INPUT_H

#include <Protocol/SimplePointer.h>

#include "Demo.h"

typedef struct _INPUT_STATE {
  DEMO                              *Demo;
  EFI_SIMPLE_TEXT_INPUT_PROTOCOL    *TextIn;
  EFI_SIMPLE_POINTER_PROTOCOL       *Pointer;
  BOUNDING_BOX                      PointerArea;
  VEC2_INTN                         PointerLocation;
} INPUT_STATE;

EFI_STATUS
EFIAPI
InputInit (
  INPUT_STATE                     *Input,
  DEMO                            *Demo,
  EFI_SIMPLE_TEXT_INPUT_PROTOCOL  *TextIn,
  EFI_SIMPLE_POINTER_PROTOCOL     *Pointer
  );

EFI_STATUS
EFIAPI
InputDispatch (
  INPUT_STATE  *Input
  );

#endif // REFINERY_INPUT_H
