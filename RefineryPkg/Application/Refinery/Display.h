#ifndef REFINERY_DISPLAY_H
#define REFINERY_DISPLAY_H

#include "Geometry.h"

EFI_STATUS
EFIAPI
ScreenBounds (
  EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL  *Out,
  BOUNDING_BOX                     *Bounds
  );

EFI_STATUS
EFIAPI
PrintBox (
  EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL  *Out,
  const BOUNDING_BOX               *Outer,
  BOUNDING_BOX                     *Inner
  );

EFI_STATUS
EFIAPI
PrintContent (
  EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL  *Out,
  const BOUNDING_BOX               *Bounds,
  const CHAR16                     *Content
  );

#endif // REFINERY_DISPLAY_H
