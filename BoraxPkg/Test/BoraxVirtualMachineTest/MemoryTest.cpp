#include "MemoryTest.hpp"

#include "BoraxVirtualMachineTest.hpp"

using ::testing::Each;
using ::testing::Not;

constexpr const BORAX_OBJECT  gSomeVal = BORAX_MAKE_FIXNUM (8675309);

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

  EXPECT_THAT (Conses1, Each (IsValidAddress (&Tracer)));
  Collect ();
  auto  Conses2 = MakeConses (1000);

  EXPECT_THAT (Conses1, Each (Not (IsValidAddress (&Tracer))));
  EXPECT_THAT (Conses2, Each (IsValidAddress (&Tracer)));
}

TEST_F (MemoryTests, RootedCons) {
  BORAX_CONS  *Cons = MakeCons ();
  AutoPin     Pin   = MakePin (Cons);

  Cons->Car = BORAX_MAKE_FIXNUM (42);
  Cons->Cdr = BORAX_MAKE_FIXNUM (77);

  Collect ();

  ASSERT_THAT (Pin.get (), IsValidAddress (&Tracer));
  ASSERT_TRUE (BORAX_IS_POINTER (Pin->Object));
  BORAX_OBJECT_HEADER  *Header = BORAX_GET_POINTER (Pin->Object);

  ASSERT_THAT (Header, IsValidAddress (&Tracer));
  ASSERT_TRUE (BORAX_IS_CONS (Header));
  BORAX_CONS  *P = reinterpret_cast<BORAX_CONS *>(Header);

  EXPECT_EQ (BORAX_MAKE_FIXNUM (42), P->Car);
  EXPECT_EQ (BORAX_MAKE_FIXNUM (77), P->Cdr);
}

TEST_F (MemoryTests, RootedList) {
  std::vector<BORAX_CONS *>  Conses = MakeConses (1000);
  AutoPin                    Pin    = MakePin (Conses[0]);

  for (size_t i = 0; i < Conses.size (); ++i) {
    Conses[i]->Car = BORAX_MAKE_FIXNUM (i);
  }

  for (size_t i = 0; i < Conses.size () - 1; ++i) {
    Conses[i]->Cdr = BORAX_MAKE_POINTER (Conses[i + 1]);
  }

  Collect ();

  ASSERT_THAT (Pin.get (), IsValidAddress (&Tracer));
  ASSERT_TRUE (BORAX_IS_POINTER (Pin->Object));
  BORAX_OBJECT_HEADER  *Header = BORAX_GET_POINTER (Pin->Object);

  ASSERT_THAT (Header, IsValidAddress (&Tracer));
  ASSERT_TRUE (BORAX_IS_CONS (Header));
  BORAX_CONS  *P = reinterpret_cast<BORAX_CONS *>(Header);

  for (size_t i = 0; i < Conses.size (); ++i) {
    EXPECT_EQ (BORAX_MAKE_FIXNUM (i), P->Car);
    if (i < Conses.size () - 1) {
      ASSERT_TRUE (BORAX_IS_POINTER (P->Cdr));
      Header = BORAX_GET_POINTER (P->Cdr);
      ASSERT_THAT (Header, IsValidAddress (&Tracer));
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

  ASSERT_THAT (Pin.get (), IsValidAddress (&Tracer));
  ASSERT_TRUE (BORAX_IS_POINTER (Pin->Object));
  BORAX_OBJECT_HEADER  *Header = BORAX_GET_POINTER (Pin->Object);

  ASSERT_THAT (Header, IsValidAddress (&Tracer));
  ASSERT_EQ (BORAX_WIDETAG_WEAK_POINTER, Header->WideTag);
  Wp = reinterpret_cast<BORAX_WEAK_POINTER *>(Header);

  EXPECT_EQ (BORAX_UNBOUND, Wp->Value);
}

TEST_F (MemoryTests, WeakPointerCanAccessAfterCollection) {
  BORAX_CONS          *Cons = MakeCons ();
  BORAX_WEAK_POINTER  *Wp   = MakeWeakPointer (Cons);
  AutoPin             Pin1  = MakePin (Wp);
  AutoPin             Pin2  = MakePin (Cons);

  Cons->Car = BORAX_MAKE_FIXNUM (343);
  Cons->Cdr = BORAX_MAKE_FIXNUM (2401);

  Collect ();

  ASSERT_THAT (Pin1.get (), IsValidAddress (&Tracer));
  ASSERT_TRUE (BORAX_IS_POINTER (Pin1->Object));
  BORAX_OBJECT_HEADER  *Header = BORAX_GET_POINTER (Pin1->Object);

  ASSERT_THAT (Header, IsValidAddress (&Tracer));
  ASSERT_EQ (BORAX_WIDETAG_WEAK_POINTER, Header->WideTag);
  Wp = reinterpret_cast<BORAX_WEAK_POINTER *>(Header);

  ASSERT_TRUE (BORAX_IS_POINTER (Wp->Value));
  Header = BORAX_GET_POINTER (Wp->Value);
  ASSERT_THAT (Header, IsValidAddress (&Tracer));
  ASSERT_TRUE (BORAX_IS_CONS (Header));
  BORAX_CONS  *P = reinterpret_cast<BORAX_CONS *>(Header);

  EXPECT_EQ (BORAX_MAKE_FIXNUM (343), P->Car);
  EXPECT_EQ (BORAX_MAKE_FIXNUM (2401), P->Cdr);
}

TEST_F (MemoryTests, RootedWordRecord) {
  BORAX_RECORD  *Record = MakeWordRecord (20, BORAX_LOWTAG_POINTER);
  AutoPin       Pin     = MakePin (Record);

  Collect ();

  ASSERT_THAT (Pin.get (), IsValidAddress (&Tracer));
  ASSERT_TRUE (BORAX_IS_POINTER (Pin->Object));
  BORAX_OBJECT_HEADER  *Header = BORAX_GET_POINTER (Pin->Object);

  ASSERT_THAT (Header, IsValidAddress (&Tracer));
  ASSERT_EQ (BORAX_WIDETAG_WORD_RECORD, Header->WideTag);

  Record = reinterpret_cast<BORAX_RECORD *>(Header);
  EXPECT_EQ (BORAX_LOWTAG_POINTER, Record->Data[0]);
  EXPECT_EQ (BORAX_LOWTAG_POINTER, Record->Data[19]);
}

TEST_F (MemoryTests, RootedObjectRecord) {
  BORAX_RECORD  *Record = MakeObjectRecord (gSomeVal, 10);
  AutoPin       Pin     = MakePin (Record);

  Collect ();

  ASSERT_THAT (Pin.get (), IsValidAddress (&Tracer));
  ASSERT_TRUE (BORAX_IS_POINTER (Pin->Object));
  BORAX_OBJECT_HEADER  *Header = BORAX_GET_POINTER (Pin->Object);

  ASSERT_THAT (Header, IsValidAddress (&Tracer));
  ASSERT_EQ (BORAX_WIDETAG_OBJECT_RECORD, Header->WideTag);
  Record = reinterpret_cast<BORAX_RECORD *>(Header);
  EXPECT_EQ (gSomeVal, Record->Class);
  EXPECT_EQ (BORAX_IMMEDIATE_UNBOUND, Record->Data[0]);
  EXPECT_EQ (BORAX_IMMEDIATE_UNBOUND, Record->Data[9]);
}

TEST_F (MemoryTests, CircularPin) {
  BORAX_PIN  *Raw1, *Raw2;

  {
    AutoPin  Pin1 = MakePin (NULL);
    AutoPin  Pin2 = MakePin (NULL);

    Raw1 = Pin1.get ();
    Raw2 = Pin2.get ();

    Raw1->Object = BORAX_MAKE_POINTER (Raw2);
    Raw2->Object = BORAX_MAKE_POINTER (Raw1);

    // Mark the pins !Live
  }

  ASSERT_THAT (Raw1, IsValidAddress (&Tracer));
  ASSERT_THAT (Raw2, IsValidAddress (&Tracer));

  Collect ();

  ASSERT_THAT (Raw1, Not (IsValidAddress (&Tracer)));
  ASSERT_THAT (Raw2, Not (IsValidAddress (&Tracer)));
}
