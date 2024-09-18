#ifndef BORAX_MEMORY_TEST_HPP
#define BORAX_MEMORY_TEST_HPP

#include <gtest/gtest.h>

extern "C" {
  #include <Library/BoraxMemory.h>
}

#include "TracingAllocator.hpp"

struct PinDeleter {
  void
  operator() (
    BORAX_PIN  *Pin
    )
  {
    BoraxReleasePin (Pin);
  }
};

using AutoPin = std::unique_ptr<BORAX_PIN, PinDeleter>;

class MemoryTests : public ::testing::Test {
public:
  TracingAllocator Tracer;
  BORAX_ALLOCATOR Alloc;

  void
  SetUp (
    ) override
  {
    BoraxAllocatorInit (&Alloc, Tracer.GetProtocol ());
  }

  void
  TearDown (
    ) override
  {
    BoraxAllocatorCleanup (&Alloc);
    ValidateReport ();
  }

  void
  ValidateReport (
    )
  {
    using ::testing::IsEmpty;

    TracingAllocator::Report  Report = Tracer.GetReport ();

    EXPECT_THAT (Report.PageAllocs, IsEmpty ());
    EXPECT_THAT (Report.PoolAllocs, IsEmpty ());
    EXPECT_THAT (Report.Errors, IsEmpty ());
  }

  void
  Collect (
    )
  {
    EFI_STATUS  Status = BoraxAllocatorCollect (&Alloc);

    EXPECT_EQ (EFI_SUCCESS, Status);
  }

  BORAX_CONS *
  MakeCons (
    BORAX_OBJECT  Car,
    BORAX_OBJECT  Cdr
    )
  {
    BORAX_CONS  *Cons;
    EFI_STATUS  Status = BoraxAllocateCons (&Alloc, Car, Cdr, &Cons);

    EXPECT_EQ (EFI_SUCCESS, Status);
    return Cons;
  }

  BORAX_CONS *
  MakeCons (
    )
  {
    return MakeCons (BORAX_UNBOUND, BORAX_UNBOUND);
  }

  std::vector<BORAX_CONS *>
  MakeConses (
    UINTN  Count
    )
  {
    std::vector<BORAX_CONS *>  Result;

    Result.reserve (Count);
    for (UINTN i = 0; i < Count; ++i) {
      BORAX_CONS  *Cons = MakeCons ();
      Result.push_back (Cons);
    }

    return Result;
  }

  std::vector<BORAX_OBJECT_HEADER *>
  MakeObjects (
    UINTN  Count
    )
  {
    std::vector<BORAX_OBJECT_HEADER *>  Result;

    Result.reserve (Count);
    for (UINTN i = 0; i < Count; ++i) {
      BORAX_OBJECT_HEADER  *Object;
      EFI_STATUS           Status = BoraxAllocateObject (
                                      &Alloc,
                                      16 * i,
                                      &Object
                                      );
      EXPECT_EQ (EFI_SUCCESS, Status);
      Result.push_back (Object);
    }

    return Result;
  }

  AutoPin
  MakePin (
    VOID  *Ptr  OPTIONAL
    )
  {
    EFI_STATUS    Status;
    BORAX_PIN     *Pin;
    BORAX_OBJECT  Object;

    if (Ptr == NULL) {
      Object = BORAX_UNBOUND;
    } else {
      Object = BORAX_MAKE_POINTER (Ptr);
    }

    Status = BoraxAllocatePin (&Alloc, Object, &Pin);
    EXPECT_EQ (EFI_SUCCESS, Status);
    return AutoPin (Pin, PinDeleter ());
  }

  template <typename Container>
  std::vector<AutoPin>
  MakePins (
    const Container  &Objects
    )
  {
    std::vector<AutoPin>  Result;

    Result.reserve (Objects.size ());
    for (auto *Object : Objects) {
      Result.push_back (MakePin (Object));
    }

    return Result;
  }

  BORAX_WEAK_POINTER *
  MakeWeakPointer (
    VOID  *Object
    )
  {
    BORAX_WEAK_POINTER  *Wp;
    EFI_STATUS          Status = BoraxAllocateWeakPointer (
                                   &Alloc,
                                   BORAX_MAKE_POINTER (Object),
                                   &Wp
                                   );

    EXPECT_EQ (EFI_SUCCESS, Status);
    return Wp;
  }

  template <typename Container>
  std::vector<BORAX_WEAK_POINTER *>
  MakeWeakPointers (
    const Container  &Objects
    )
  {
    std::vector<BORAX_WEAK_POINTER *>  Result;

    Result.reserve (Objects.size ());
    for (auto *Object : Objects) {
      BORAX_WEAK_POINTER  *Wp = MakeWeakPointer (Object);
      Result.push_back (Wp);
    }

    return Result;
  }

  BORAX_RECORD *
  MakeWordRecord (
    UINTN  Length,
    UINTN  InitialElement
    )
  {
    BORAX_RECORD  *Record;
    EFI_STATUS    Status;

    Status = BoraxAllocateRecord (
               &Alloc,
               BORAX_WIDETAG_WORD_RECORD,
               BORAX_UNBOUND,  // Class
               Length,
               0,  // LengthAux
               InitialElement,
               &Record
               );
    EXPECT_EQ (EFI_SUCCESS, Status);
    return Record;
  }

  BORAX_RECORD *
  MakeObjectRecord (
    BORAX_OBJECT  Class,
    UINTN         Length
    )
  {
    BORAX_RECORD  *Record;
    EFI_STATUS    Status;

    Status = BoraxAllocateRecord (
               &Alloc,
               BORAX_WIDETAG_OBJECT_RECORD,
               Class,
               Length,
               0,                       // LengthAux
               BORAX_IMMEDIATE_UNBOUND, // InitialElement
               &Record
               );
    EXPECT_EQ (EFI_SUCCESS, Status);
    return Record;
  }
};

#endif // BORAX_MEMORY_TEST_HPP
