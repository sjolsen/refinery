#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include "TracingAllocator.hpp"

using ::testing::IsEmpty;
using ::testing::Each;
using ::testing::Not;

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
    return MakeCons (BORAX_IMMEDIATE_UNBOUND, BORAX_IMMEDIATE_UNBOUND);
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
    VOID  *Object
    )
  {
    BORAX_PIN   *Pin;
    EFI_STATUS  Status = BoraxAllocatePin (
                           &Alloc,
                           BORAX_MAKE_POINTER (Object),
                           &Pin
                           );

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
               BORAX_IMMEDIATE_UNBOUND,  // Class
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

constexpr const BORAX_OBJECT  gSomeVal = 8675309 << 1; // fixnum

TEST_F (MemoryTests, CleanupNothing) {
}

TEST_F (MemoryTests, CleanupCons) {
  (VOID)MakeConses (4000);
}

TEST_F (MemoryTests, CleanupObject) {
  (VOID)MakeObjects (100);
}

TEST_F (MemoryTests, CleanupPin) {
  auto  Conses = MakeConses (10);

  (VOID)MakePins (Conses);
}

TEST_F (MemoryTests, CleanupWeakPointer) {
  auto  Conses = MakeConses (1000);

  (VOID)MakeWeakPointers (Conses);
}

TEST_F (MemoryTests, CleanupWordRecord) {
  (VOID)MakeWordRecord (1000, BORAX_LOWTAG_POINTER);
}

TEST_F (MemoryTests, CleanupObjectRecord) {
  (VOID)MakeObjectRecord (gSomeVal, 20);
}

TEST_F (MemoryTests, CollectNothing) {
  Collect ();
}

TEST_F (MemoryTests, CollectRootlessCons) {
  (VOID)MakeConses (4000);
  Collect ();
}

TEST_F (MemoryTests, CollectRootlessObject) {
  (VOID)MakeObjects (100);
  Collect ();
}

TEST_F (MemoryTests, CollectRootlessPin) {
  auto  Conses = MakeConses (10);

  (VOID)MakePins (Conses);
  Collect ();
}

TEST_F (MemoryTests, CollectRootlessWeakPointer) {
  auto  Conses = MakeConses (1000);

  (VOID)MakeWeakPointers (Conses);
  Collect ();
}

TEST_F (MemoryTests, CollectRootlessWordRecord) {
  (VOID)MakeWordRecord (1000, BORAX_LOWTAG_POINTER);
  Collect ();
}

TEST_F (MemoryTests, CleanupRootlessObjectRecord) {
  (VOID)MakeObjectRecord (gSomeVal, 20);
  Collect ();
}

TEST_F (MemoryTests, IsValidSanityCheck) {
  auto  Conses1 = MakeConses (1000);

  EXPECT_THAT (Conses1, Each (IsValidAddress (Tracer)));
  Collect ();
  auto  Conses2 = MakeConses (1000);

  EXPECT_THAT (Conses1, Each (Not (IsValidAddress (Tracer))));
  EXPECT_THAT (Conses2, Each (IsValidAddress (Tracer)));
}

TEST_F (MemoryTests, RootedCons) {
  BORAX_CONS  *Cons = MakeCons ();
  AutoPin     Pin   = MakePin (Cons);

  Cons->Car = 42 << 1;  // fixnums
  Cons->Cdr = 77 << 1;

  Collect ();

  ASSERT_THAT (Pin.get (), IsValidAddress (Tracer));
  ASSERT_TRUE (BORAX_IS_POINTER (Pin->Object));
  BORAX_OBJECT_HEADER  *Header = BORAX_GET_POINTER (Pin->Object);

  ASSERT_THAT (Header, IsValidAddress (Tracer));
  ASSERT_TRUE (BORAX_IS_CONS (Header));
  BORAX_CONS  *P = reinterpret_cast<BORAX_CONS *>(Header);

  EXPECT_EQ ((UINTN)(42 << 1), P->Car);
  EXPECT_EQ ((UINTN)(77 << 1), P->Cdr);
}

TEST_F (MemoryTests, RootedList) {
  std::vector<BORAX_CONS *>  Conses = MakeConses (1000);
  AutoPin                    Pin    = MakePin (Conses[0]);

  for (size_t i = 0; i < Conses.size (); ++i) {
    Conses[i]->Car = i << 1;  // fixnum
  }

  for (size_t i = 0; i < Conses.size () - 1; ++i) {
    Conses[i]->Cdr = BORAX_MAKE_POINTER (Conses[i + 1]);
  }

  Collect ();

  ASSERT_THAT (Pin.get (), IsValidAddress (Tracer));
  ASSERT_TRUE (BORAX_IS_POINTER (Pin->Object));
  BORAX_OBJECT_HEADER  *Header = BORAX_GET_POINTER (Pin->Object);

  ASSERT_THAT (Header, IsValidAddress (Tracer));
  ASSERT_TRUE (BORAX_IS_CONS (Header));
  BORAX_CONS  *P = reinterpret_cast<BORAX_CONS *>(Header);

  for (size_t i = 0; i < Conses.size (); ++i) {
    EXPECT_EQ (i << 1, P->Car);
    if (i < Conses.size () - 1) {
      ASSERT_TRUE (BORAX_IS_POINTER (P->Cdr));
      Header = BORAX_GET_POINTER (P->Cdr);
      ASSERT_THAT (Header, IsValidAddress (Tracer));
      ASSERT_TRUE (BORAX_IS_CONS (Header));
      P = reinterpret_cast<BORAX_CONS *>(Header);
    } else {
      ASSERT_FALSE (BORAX_IS_POINTER (P->Cdr));
    }
  }
}

TEST_F (MemoryTests, WeakPointerIsWeak) {
  BORAX_CONS          *Cons = MakeCons ();
  BORAX_WEAK_POINTER  *Wp   = MakeWeakPointer (Cons);
  AutoPin             Pin   = MakePin (Wp);

  Collect ();

  ASSERT_THAT (Pin.get (), IsValidAddress (Tracer));
  ASSERT_TRUE (BORAX_IS_POINTER (Pin->Object));
  BORAX_OBJECT_HEADER  *Header = BORAX_GET_POINTER (Pin->Object);

  ASSERT_THAT (Header, IsValidAddress (Tracer));
  ASSERT_EQ (BORAX_WIDETAG_WEAK_POINTER, Header->WideTag);
  Wp = reinterpret_cast<BORAX_WEAK_POINTER *>(Header);

  EXPECT_EQ (BORAX_IMMEDIATE_UNBOUND, Wp->Value);
}

TEST_F (MemoryTests, WeakPointerCanAccessAfterCollection) {
  BORAX_CONS          *Cons = MakeCons ();
  BORAX_WEAK_POINTER  *Wp   = MakeWeakPointer (Cons);
  AutoPin             Pin1  = MakePin (Wp);
  AutoPin             Pin2  = MakePin (Cons);

  Cons->Car = 343 << 1;  // fixnum
  Cons->Cdr = 2401 << 1;

  Collect ();

  ASSERT_THAT (Pin1.get (), IsValidAddress (Tracer));
  ASSERT_TRUE (BORAX_IS_POINTER (Pin1->Object));
  BORAX_OBJECT_HEADER  *Header = BORAX_GET_POINTER (Pin1->Object);

  ASSERT_THAT (Header, IsValidAddress (Tracer));
  ASSERT_EQ (BORAX_WIDETAG_WEAK_POINTER, Header->WideTag);
  Wp = reinterpret_cast<BORAX_WEAK_POINTER *>(Header);

  ASSERT_TRUE (BORAX_IS_POINTER (Wp->Value));
  Header = BORAX_GET_POINTER (Wp->Value);
  ASSERT_THAT (Header, IsValidAddress (Tracer));
  ASSERT_TRUE (BORAX_IS_CONS (Header));
  BORAX_CONS  *P = reinterpret_cast<BORAX_CONS *>(Header);

  EXPECT_EQ ((UINTN)(343 << 1), P->Car);
  EXPECT_EQ ((UINTN)(2401 << 1), P->Cdr);
}

TEST_F (MemoryTests, RootedWordRecord) {
  BORAX_RECORD  *Record = MakeWordRecord (20, BORAX_LOWTAG_POINTER);
  AutoPin       Pin     = MakePin (Record);

  Collect ();

  ASSERT_THAT (Pin.get (), IsValidAddress (Tracer));
  ASSERT_TRUE (BORAX_IS_POINTER (Pin->Object));
  BORAX_OBJECT_HEADER  *Header = BORAX_GET_POINTER (Pin->Object);

  ASSERT_THAT (Header, IsValidAddress (Tracer));
  ASSERT_EQ (BORAX_WIDETAG_WORD_RECORD, Header->WideTag);

  Record = reinterpret_cast<BORAX_RECORD *>(Header);
  EXPECT_EQ (BORAX_LOWTAG_POINTER, Record->Data[0]);
  EXPECT_EQ (BORAX_LOWTAG_POINTER, Record->Data[19]);
}

TEST_F (MemoryTests, RootedObjectRecord) {
  BORAX_RECORD  *Record = MakeObjectRecord (gSomeVal, 10);
  AutoPin       Pin     = MakePin (Record);

  Collect ();

  ASSERT_THAT (Pin.get (), IsValidAddress (Tracer));
  ASSERT_TRUE (BORAX_IS_POINTER (Pin->Object));
  BORAX_OBJECT_HEADER  *Header = BORAX_GET_POINTER (Pin->Object);

  ASSERT_THAT (Header, IsValidAddress (Tracer));
  ASSERT_EQ (BORAX_WIDETAG_OBJECT_RECORD, Header->WideTag);
  Record = reinterpret_cast<BORAX_RECORD *>(Header);
  EXPECT_EQ (gSomeVal, Record->Class);
  EXPECT_EQ (BORAX_IMMEDIATE_UNBOUND, Record->Data[0]);
  EXPECT_EQ (BORAX_IMMEDIATE_UNBOUND, Record->Data[9]);
}

int
main (
  int   argc,
  char  *argv[]
  )
{
  testing::InitGoogleTest (&argc, argv);
  return RUN_ALL_TESTS ();
}
