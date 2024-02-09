#include <unordered_map>

#include <gmock/gmock.h>
#include <gtest/gtest.h>

extern "C" {
  #include <Library/BaseLib.h>
  #include <Library/BaseMemoryLib.h>
  #include <Library/BoraxMemory.h>
}

using ::testing::IsEmpty;
using ::testing::Each;
using ::testing::Not;

#define WRAP_FN(_memfn)                               \
[](auto This, auto... Args) [[gnu::ms_abi]] -> auto { \
  return _Cast(This)->_memfn(Args...);                \
}

class TracingAllocator {
public:
  struct PageAllocation {
    VOID     *Address;
    UINTN    Pages;
  };

  struct PoolAllocation {
    VOID     *Address;
    UINTN    Size;
  };

  struct Report {
    std::vector<PageAllocation>    PageAllocs;
    std::vector<PoolAllocation>    PoolAllocs;
    std::vector<std::string>       Errors;
  };

private:
  BORAX_SYSTEM_ALLOCATOR_PROTOCOL _Protocol = {
    .AllocatePages = WRAP_FN (_AllocatePages),
    .FreePages     = WRAP_FN (_FreePages),
    .AllocatePool  = WRAP_FN (_AllocatePool),
    .FreePool      = WRAP_FN (_FreePool),
  };

  std::unordered_map<UINTN, PageAllocation> _PageAllocs;
  std::unordered_map<UINTN, PoolAllocation> _PoolAllocs;
  std::vector<std::string> _Errors;

  static TracingAllocator *
  _Cast (
    BORAX_SYSTEM_ALLOCATOR_PROTOCOL  *This
    )
  {
    return BASE_CR (This, TracingAllocator, _Protocol);
  }

  VOID *
  _AllocatePages (
    UINTN  Pages
    )
  {
    VOID  *Mem = aligned_alloc (
                   BORAX_PAGE_SIZE,
                   Pages * BORAX_PAGE_SIZE
                   );
    PageAllocation  Record = { Mem, Pages };

    _PageAllocs.emplace ((UINTN)Mem, Record);
    return Mem;
  }

  VOID
  _FreePages (
    VOID   *Buffer,
    UINTN  Pages
    )
  {
    auto  It = _PageAllocs.find ((UINTN)Buffer);

    if (It == _PageAllocs.end ()) {
      std::stringstream  ss;
      ss << "Spurious FreePages of " << Pages << " pages at "
      << "0x" << std::hex << (UINTN)Buffer;
      _Errors.push_back (std::move (ss).str ());
    } else {
      PageAllocation  &Record = It->second;
      if (Pages != Record.Pages) {
        std::stringstream  ss;
        ss << "FreePages called with wrong number of pages "
        << "(expected " << Record.Pages << "; got "
        << Pages << ") at "
        << "0x" << std::hex << (UINTN)Buffer;
        _Errors.push_back (std::move (ss).str ());
      }

      _PageAllocs.erase (It);
      free (Buffer);
    }
  }

  VOID *
  _AllocatePool (
    UINTN  AllocationSize
    )
  {
    VOID            *Mem   = malloc (AllocationSize);
    PoolAllocation  Record = { Mem, AllocationSize };

    _PoolAllocs.emplace ((UINTN)Mem, Record);
    return Mem;
  }

  VOID
  _FreePool (
    VOID  *Buffer
    )
  {
    auto  It = _PoolAllocs.find ((UINTN)Buffer);

    if (It == _PoolAllocs.end ()) {
      std::stringstream  ss;
      ss << "Spurious FreePool at "
      << "0x" << std::hex << (UINTN)Buffer;
      _Errors.push_back (std::move (ss).str ());
    } else {
      _PoolAllocs.erase (It);
      free (Buffer);
    }
  }

public:
  BORAX_SYSTEM_ALLOCATOR_PROTOCOL *
  GetProtocol (
    )
  {
    return &_Protocol;
  }

  Report
  GetReport (
    ) const
  {
    Report  Result;

    for (auto [_, PageAlloc] : _PageAllocs) {
      Result.PageAllocs.push_back (PageAlloc);
    }

    for (auto [_, PoolAlloc] : _PoolAllocs) {
      Result.PoolAllocs.push_back (PoolAlloc);
    }

    Result.Errors = _Errors;
    return Result;
  }

  bool
  IsValidAddress (
    VOID  *Ptr
    ) const
  {
    auto  Address = (UINTN)Ptr;

    for (auto [_, PageAlloc] : _PageAllocs) {
      auto  Begin = (UINTN)PageAlloc.Address;
      auto  End   = Begin + BORAX_PAGE_SIZE * PageAlloc.Pages;
      if ((Begin <= Address) && (Address < End)) {
        return true;
      }
    }

    for (auto [_, PoolAlloc] : _PoolAllocs) {
      auto  Begin = (UINTN)PoolAlloc.Address;
      auto  End   = Begin + PoolAlloc.Size;
      if ((Begin <= Address) && (Address < End)) {
        return true;
      }
    }

    return false;
  }
};

std::ostream &
operator<< (
  std::ostream                            &os,
  const TracingAllocator::PageAllocation  &PageAlloc
  )
{
  os << "PageAllocation { "
  << "0x" << std::hex << (UINTN)PageAlloc.Address
  << ", " << std::dec << PageAlloc.Pages
  << " }";
  return os;
}

std::ostream &
operator<< (
  std::ostream                            &os,
  const TracingAllocator::PoolAllocation  &PoolAlloc
  )
{
  os << "PoolAllocation { "
  << "0x" << std::hex << (UINTN)PoolAlloc.Address
  << ", " << std::dec << PoolAlloc.Size
  << " }";
  return os;
}

std::ostream &
operator<< (
  std::ostream                    &os,
  const TracingAllocator::Report  &Report
  )
{
  for (auto PageAlloc : Report.PageAllocs) {
    os << PageAlloc << std::endl;
  }

  for (auto PoolAlloc : Report.PoolAllocs) {
    os << PoolAlloc << std::endl;
  }

  for (const std::string &Error : Report.Errors) {
    os << "Error: " << Error << std::endl;
  }

  return os;
}

MATCHER_P (IsValidAddress, Alloc, "is a valid address") {
  return Alloc.IsValidAddress (arg);
}

template <typename I>
class RootSetIterator {
private:
  BORAX_ROOTSET_ITERATOR _Handle;
  I _Begin;
  I _End;

  static BOOLEAN
  EFIAPI
  _ConsumeOne (
    VOID          *Ctx,
    BORAX_OBJECT  **Object
    )
  {
    auto  This = static_cast<RootSetIterator *>(Ctx);

    if (This->_Begin == This->_End) {
      return FALSE;
    } else {
      *Object = *This->_Begin++;
      return TRUE;
    }
  }

public:
  RootSetIterator(
                  I  Begin,
                  I  End
                  )
    : _Handle {this, _ConsumeOne},
    _Begin {std::move (Begin)},
    _End {std::move (End)}
  {
  }

  BORAX_ROOTSET_ITERATOR *
  Get (
    )
  {
    return &_Handle;
  }
};

template <typename Container>
RootSetIterator<typename Container::const_iterator>
MakeRootSetIterator (
  const Container  &C
  )
{
  using std::begin;
  using std::end;
  return RootSetIterator (begin (C), end (C));
}

class MemoryLeakTests : public ::testing::Test {
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

  template <typename Container>
  void
  Collect (
    const Container  &RootSet
    )
  {
    auto        RSI    = MakeRootSetIterator (RootSet);
    EFI_STATUS  Status = BoraxAllocatorCollect (&Alloc, RSI.Get ());

    EXPECT_EQ (EFI_SUCCESS, Status);
  }

  void
  Collect (
    )
  {
    Collect (std::initializer_list<BORAX_OBJECT *>{ });
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

  BORAX_PIN *
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
    return Pin;
  }

  template <typename Container>
  std::vector<BORAX_PIN *>
  MakePins (
    const Container  &Objects
    )
  {
    std::vector<BORAX_PIN *>  Result;

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

  BORAX_STRING *
  MakeString (
    CONST CHAR16  *Text
    )
  {
    BORAX_STRING  *String;
    EFI_STATUS    Status;
    UINTN         Length = StrLen (Text) + 1; // NUL-terminated

    Status = BoraxAllocateString (&Alloc, Length, &String);
    EXPECT_EQ (EFI_SUCCESS, Status);
    CopyMem (String->Data, Text, 2 * Length);
    return String;
  }

  BORAX_STRING *
  MakeString (
    const wchar_t  *Text
    )
  {
    return MakeString ((CONST CHAR16 *)Text);
  }

  BORAX_VECTOR *
  MakeVector (
    UINTN         Length,
    BORAX_OBJECT  InitialElement
    )
  {
    BORAX_VECTOR  *Vector;
    EFI_STATUS    Status;

    Status = BoraxAllocateVector (&Alloc, Length, InitialElement, &Vector);
    EXPECT_EQ (EFI_SUCCESS, Status);
    return Vector;
  }

  BORAX_RECORD *
  MakeRecord (
    BORAX_OBJECT  Type,
    UINTN         Length
    )
  {
    BORAX_RECORD  *Record;
    EFI_STATUS    Status;

    Status = BoraxAllocateRecord (&Alloc, Type, Length, &Record);
    EXPECT_EQ (EFI_SUCCESS, Status);
    return Record;
  }
};

constexpr const BORAX_OBJECT  gSomeVal = 8675309 << 1; // fixnum

TEST_F (MemoryLeakTests, CleanupNothing) {
}

TEST_F (MemoryLeakTests, CleanupCons) {
  (VOID)MakeConses (4000);
}

TEST_F (MemoryLeakTests, CleanupObject) {
  (VOID)MakeObjects (100);
}

TEST_F (MemoryLeakTests, CleanupPin) {
  auto  Conses = MakeConses (10);

  (VOID)MakePins (Conses);
}

TEST_F (MemoryLeakTests, CleanupWeakPointer) {
  auto  Conses = MakeConses (1000);

  (VOID)MakeWeakPointers (Conses);
}

TEST_F (MemoryLeakTests, CleanupString) {
  (VOID)MakeString (L"Hello, world!");
}

TEST_F (MemoryLeakTests, CleanupVector) {
  (VOID)MakeVector (1000, gSomeVal);
}

TEST_F (MemoryLeakTests, CleanupRecord) {
  (VOID)MakeRecord (gSomeVal, 20);
}

TEST_F (MemoryLeakTests, CollectNothing) {
  Collect ();
}

TEST_F (MemoryLeakTests, CollectRootlessCons) {
  (VOID)MakeConses (4000);
  Collect ();
}

TEST_F (MemoryLeakTests, CollectRootlessObject) {
  (VOID)MakeObjects (100);
  Collect ();
}

TEST_F (MemoryLeakTests, CollectRootlessPin) {
  auto  Conses = MakeConses (10);

  (VOID)MakePins (Conses);
  Collect ();
}

TEST_F (MemoryLeakTests, CollectRootlessWeakPointer) {
  auto  Conses = MakeConses (1000);

  (VOID)MakeWeakPointers (Conses);
  Collect ();
}

TEST_F (MemoryLeakTests, CollectRootlessString) {
  (VOID)MakeString (L"Hello, world!");
  Collect ();
}

TEST_F (MemoryLeakTests, CollectRootlessVector) {
  (VOID)MakeVector (1000, gSomeVal);
  Collect ();
}

TEST_F (MemoryLeakTests, CleanupRootlessRecord) {
  (VOID)MakeRecord (gSomeVal, 20);
  Collect ();
}

TEST_F (MemoryLeakTests, IsValidSanityCheck) {
  auto  Conses1 = MakeConses (1000);

  EXPECT_THAT (Conses1, Each (IsValidAddress (Tracer)));
  Collect ();
  auto  Conses2 = MakeConses (1000);

  EXPECT_THAT (Conses1, Each (Not (IsValidAddress (Tracer))));
  EXPECT_THAT (Conses2, Each (IsValidAddress (Tracer)));
}

TEST_F (MemoryLeakTests, RootedCons) {
  BORAX_CONS  *Cons = MakeCons ();

  Cons->Car = 42 << 1;  // fixnums
  Cons->Cdr = 77 << 1;

  BORAX_OBJECT                 Root  = BORAX_MAKE_POINTER (Cons);
  std::vector<BORAX_OBJECT *>  Roots = { &Root };

  Collect (Roots);

  ASSERT_TRUE (BORAX_IS_POINTER (Root));
  BORAX_OBJECT_HEADER  *Header = BORAX_GET_POINTER (Root);

  ASSERT_THAT (Header, IsValidAddress (Tracer));
  ASSERT_TRUE (BORAX_IS_CONS (Header));
  BORAX_CONS  *P = reinterpret_cast<BORAX_CONS *>(Header);

  EXPECT_EQ ((UINTN)(42 << 1), P->Car);
  EXPECT_EQ ((UINTN)(77 << 1), P->Cdr);
}

TEST_F (MemoryLeakTests, RootedList) {
  std::vector<BORAX_CONS *>  Conses = MakeConses (1000);

  for (size_t i = 0; i < Conses.size (); ++i) {
    Conses[i]->Car = i << 1;  // fixnum
  }

  for (size_t i = 0; i < Conses.size () - 1; ++i) {
    Conses[i]->Cdr = BORAX_MAKE_POINTER (Conses[i + 1]);
  }

  BORAX_OBJECT                 Root  = BORAX_MAKE_POINTER (Conses[0]);
  std::vector<BORAX_OBJECT *>  Roots = { &Root };

  Collect (Roots);

  ASSERT_TRUE (BORAX_IS_POINTER (Root));
  BORAX_OBJECT_HEADER  *Header = BORAX_GET_POINTER (Root);

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

TEST_F (MemoryLeakTests, WeakPointerIsWeak) {
  BORAX_CONS                   *Cons = MakeCons ();
  BORAX_WEAK_POINTER           *Wp   = MakeWeakPointer (Cons);
  BORAX_OBJECT                 Root  = BORAX_MAKE_POINTER (Wp);
  std::vector<BORAX_OBJECT *>  Roots = { &Root };

  Collect (Roots);

  ASSERT_TRUE (BORAX_IS_POINTER (Root));
  BORAX_OBJECT_HEADER  *Header = BORAX_GET_POINTER (Root);

  ASSERT_THAT (Header, IsValidAddress (Tracer));
  ASSERT_EQ (BORAX_WIDETAG_WEAK_POINTER, Header->WideTag);
  Wp = reinterpret_cast<BORAX_WEAK_POINTER *>(Header);

  EXPECT_EQ (BORAX_IMMEDIATE_UNBOUND, Wp->Value);
}

TEST_F (MemoryLeakTests, WeakPointerCanAccessAfterCollection) {
  BORAX_CONS          *Cons = MakeCons ();
  BORAX_WEAK_POINTER  *Wp   = MakeWeakPointer (Cons);
  BORAX_OBJECT        Root1 = BORAX_MAKE_POINTER (Wp);
  BORAX_OBJECT        Root2 = BORAX_MAKE_POINTER (Cons);

  Cons->Car = 343 << 1;  // fixnum
  Cons->Cdr = 2401 << 1;

  std::vector<BORAX_OBJECT *>  Roots = { &Root1, &Root2 };

  Collect (Roots);

  ASSERT_TRUE (BORAX_IS_POINTER (Root1));
  BORAX_OBJECT_HEADER  *Header = BORAX_GET_POINTER (Root1);

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

TEST_F (MemoryLeakTests, RootedString) {
  BORAX_STRING                 *String = MakeString (L"Hello, world!");
  BORAX_OBJECT                 Root    = BORAX_MAKE_POINTER (String);
  std::vector<BORAX_OBJECT *>  Roots   = { &Root };

  Collect (Roots);

  ASSERT_TRUE (BORAX_IS_POINTER (Root));
  BORAX_OBJECT_HEADER  *Header = BORAX_GET_POINTER (Root);

  ASSERT_THAT (Header, IsValidAddress (Tracer));
  ASSERT_EQ (BORAX_WIDETAG_STRING, Header->WideTag);
  String = reinterpret_cast<BORAX_STRING *>(Header);
  EXPECT_EQ (0, StrCmp ((CONST CHAR16 *)L"Hello, world!", String->Data));
}

TEST_F (MemoryLeakTests, RootedVector) {
  BORAX_VECTOR                 *Vector = MakeVector (100, 42 << 1);
  BORAX_OBJECT                 Root    = BORAX_MAKE_POINTER (Vector);
  std::vector<BORAX_OBJECT *>  Roots   = { &Root };

  Collect (Roots);

  ASSERT_TRUE (BORAX_IS_POINTER (Root));
  BORAX_OBJECT_HEADER  *Header = BORAX_GET_POINTER (Root);

  ASSERT_THAT (Header, IsValidAddress (Tracer));
  ASSERT_EQ (BORAX_WIDETAG_VECTOR, Header->WideTag);
  Vector = reinterpret_cast<BORAX_VECTOR *>(Header);
  EXPECT_EQ ((UINTN)(42 << 1), Vector->Data[0]);
  EXPECT_EQ ((UINTN)(42 << 1), Vector->Data[99]);
}

TEST_F (MemoryLeakTests, RootedRecord) {
  BORAX_RECORD                 *Record = MakeRecord (gSomeVal, 10);
  BORAX_OBJECT                 Root    = BORAX_MAKE_POINTER (Record);
  std::vector<BORAX_OBJECT *>  Roots   = { &Root };

  Collect (Roots);

  ASSERT_TRUE (BORAX_IS_POINTER (Root));
  BORAX_OBJECT_HEADER  *Header = BORAX_GET_POINTER (Root);

  ASSERT_THAT (Header, IsValidAddress (Tracer));
  ASSERT_EQ (BORAX_WIDETAG_RECORD, Header->WideTag);
  Record = reinterpret_cast<BORAX_RECORD *>(Header);
  EXPECT_EQ (gSomeVal, Record->Type);
  EXPECT_EQ (BORAX_IMMEDIATE_UNBOUND, Record->Slots[0]);
  EXPECT_EQ (BORAX_IMMEDIATE_UNBOUND, Record->Slots[9]);
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
