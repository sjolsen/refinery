#include "Geometry.h"

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

VOID
EFIAPI
ClampVector (
  const BOUNDING_BOX  *Area,
  VEC2_INTN           *Vector
  )
{
  Vector->X = MAX (0, MIN (Area->XMax - 1, Vector->X));
  Vector->Y = MAX (0, MIN (Area->YMax - 1, Vector->Y));
}

VOID
EFIAPI
RebaseVector (
  const BOUNDING_BOX  *AreaIn,
  const VEC2_INTN     *VectorIn,
  const BOUNDING_BOX  *AreaOut,
  VEC2_INTN           *VectorOut
  )
{
  INTN  XIn     = VectorIn->X - AreaIn->XMin;
  INTN  YIn     = VectorIn->Y - AreaIn->YMin;
  INTN  XInMax  = AreaIn->XMax - AreaIn->XMin;
  INTN  YInMax  = AreaIn->YMax - AreaIn->YMin;
  INTN  XOutMax = AreaOut->XMax - AreaOut->XMin;
  INTN  YOutMax = AreaOut->YMax - AreaOut->YMin;
  INTN  XOut    = (XIn * XOutMax) / XInMax;
  INTN  YOut    = (YIn * YOutMax) / YInMax;

  VectorOut->X = AreaOut->XMin + XOut;
  VectorOut->Y = AreaOut->YMin + YOut;
}
