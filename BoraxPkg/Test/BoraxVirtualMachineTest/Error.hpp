#ifndef BORAX_ERROR_HPP
#define BORAX_ERROR_HPP

#include <exception>
#include <string>

extern "C" {
  #include <Uefi.h>
}

#include "MemoryTest.hpp"

namespace borax::testing {
class Error : public std::exception {
protected:
  std::string WhatStr_;

public:
  Error (
         ) = default;

  Error (
         std::string  WhatStr
         )
    : WhatStr_ (std::move (WhatStr))
  {
  }

  const char *
  what (
    ) const noexcept override final
  {
    return WhatStr_.c_str ();
  }
};

class EFIError : public Error {
protected:
  EFI_STATUS Status_;

public:
  EFIError (
            IN EFI_STATUS  Status
            );

  EFI_STATUS
  Status (
    ) const noexcept
  {
    return Status_;
  }
};

class ConditionError : public Error {
protected:
  AutoPin Condition_;

  static
  AutoPin
  PinCondition (
    IN BORAX_ALLOCATOR  *Alloc,
    IN BORAX_OBJECT     Condition
    );

public:
  ConditionError (
                  IN BORAX_ALLOCATOR  *Alloc,
                  IN BORAX_OBJECT     Condition
                  );

  BORAX_OBJECT
  Condition (
    ) const noexcept
  {
    return Condition_->Object;
  }
};
}  // namespace borax::testing

#endif // BORAX_ERROR_HPP
