#ifndef REFINERY_IMAGE_H
#define REFINERY_IMAGE_H

#include "Buffer.h"

EFI_STATUS
EFIAPI
ImageLoadContent (
  IN OUT BUFFER  *Content
  );

#endif // REFINERY_IMAGE_H
