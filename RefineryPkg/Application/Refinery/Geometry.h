#ifndef REFINERY_GEOMETRY_H
#define REFINERY_GEOMETRY_H

typedef struct _BOUNDING_BOX {
  INTN    XMin, XMax;
  INTN    YMin, YMax;
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
  );

typedef struct _VEC2_UINTN {
  INTN    X, Y;
} VEC2_INTN;

VOID
EFIAPI
ClampVector (
  const BOUNDING_BOX  *Area,
  VEC2_INTN           *Vector
  );

VOID
EFIAPI
RebaseVector (
  const BOUNDING_BOX  *AreaIn,
  const VEC2_INTN     *VectorIn,
  const BOUNDING_BOX  *AreaOut,
  VEC2_INTN           *VectorOut
  );

#endif // REFINERY_GEOMETRY_H
