#ifndef REFINERY_DEMO_H
#define REFINERY_DEMO_H

#include "Geometry.h"

typedef enum _DEMO_STATE {
  DEMO_STATE_RUNNING,
  DEMO_STATE_EXIT,
  DEMO_STATE_SHUTDOWN,
} DEMO_STATE;

typedef enum _DEMO_REDRAW {
  DEMO_REDRAW_CONTENT = 0x01,
  DEMO_REDRAW_CURSOR  = 0x02,
} DEMO_REDRAW;

typedef struct _DEMO {
  DEMO_STATE                         State;
  DEMO_REDRAW                        Redraw;
  EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL    *TextOut;
  UINTN                              Background;
  UINTN                              Foreground;
  BOUNDING_BOX                       ScreenBounds;
  VEC2_INTN                          Cursor;
  const CHAR16                       *Content;
} DEMO;

EFI_STATUS
EFIAPI
DemoInit (
  DEMO                             *Demo,
  EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL  *TextOut
  );

EFI_STATUS
EFIAPI
DemoRedraw (
  DEMO  *Demo
  );

EFI_STATUS
EFIAPI
DemoHandleKeyboard (
  DEMO                 *Demo,
  const EFI_INPUT_KEY  *Key
  );

EFI_STATUS
EFIAPI
DemoHandlePointer (
  DEMO                *Demo,
  const BOUNDING_BOX  *PointerArea,
  const VEC2_INTN     *Pointer
  );

#endif // REFINERY_DEMO_H
