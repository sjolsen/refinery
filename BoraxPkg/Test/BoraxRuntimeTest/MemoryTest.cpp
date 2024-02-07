#include <unordered_map>

#include <gmock/gmock.h>
#include <gtest/gtest.h>

extern "C" {
  #include <Library/BoraxMemory.h>
}

using ::testing::IsEmpty;

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
    VOID            *Mem   = malloc (Pages * BORAX_PAGE_SIZE);
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
    BORAX_OBJECT  *Object
    )
  {
    auto  This = static_cast<RootSetIterator*>(Ctx);

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

  BORAX_ROOTSET_ITERATOR *Get() { return &_Handle; }
};

template <typename Container>
RootSetIterator<typename Container::const_iterator>
MakeRootSetIterator(const Container& C) {
  using std::begin;
  using std::end;
  return RootSetIterator(begin(C), end(C));
}

class MemoryLeakTests : public ::testing::Test {
public:
  TracingAllocator Tracer;

  void
  ValidateReport (
    )
  {
    TracingAllocator::Report  Report = Tracer.GetReport ();

    EXPECT_THAT (Report.PageAllocs, IsEmpty ());
    EXPECT_THAT (Report.PoolAllocs, IsEmpty ());
    EXPECT_THAT (Report.Errors, IsEmpty ());
  }
};

TEST_F (MemoryLeakTests, CleanupNothing) {
  BORAX_ALLOCATOR  Alloc;

  BoraxAllocatorInit (&Alloc, Tracer.GetProtocol ());
  BoraxAllocatorCleanup (&Alloc);
  ValidateReport ();
}

TEST_F (MemoryLeakTests, CleanupCons) {
  EFI_STATUS       Status;
  BORAX_ALLOCATOR  Alloc;

  BoraxAllocatorInit (&Alloc, Tracer.GetProtocol ());

  for (int i = 0; i < 4000; ++i) {
    BORAX_CONS *Cons;
    Status = BoraxAllocateCons(&Alloc, &Cons);
    EXPECT_EQ (EFI_SUCCESS, Status);
  }

  BoraxAllocatorCleanup (&Alloc);
  ValidateReport ();
}

TEST_F (MemoryLeakTests, CleanupObject) {
  EFI_STATUS       Status;
  BORAX_ALLOCATOR  Alloc;

  BoraxAllocatorInit (&Alloc, Tracer.GetProtocol ());

  for (int i = 0; i < 100; ++i) {
    BORAX_OBJECT_HEADER *Object;
    Status = BoraxAllocateObject(&Alloc, 16 * i, &Object);
    EXPECT_EQ (EFI_SUCCESS, Status);
  }

  BoraxAllocatorCleanup (&Alloc);
  ValidateReport ();
}

TEST_F (MemoryLeakTests, CleanupPin) {
  EFI_STATUS       Status;
  BORAX_ALLOCATOR  Alloc;

  BoraxAllocatorInit (&Alloc, Tracer.GetProtocol ());

  for (int i = 0; i < 10; ++i) {
    BORAX_CONS *Cons;
    Status = BoraxAllocateCons(&Alloc, &Cons);
    EXPECT_EQ (EFI_SUCCESS, Status);

    BORAX_PIN *Pin;
    Status = BoraxAllocatePin(&Alloc, BORAX_MAKE_POINTER(Cons), &Pin);
    EXPECT_EQ (EFI_SUCCESS, Status);
  }

  BoraxAllocatorCleanup (&Alloc);
  ValidateReport ();
}

TEST_F (MemoryLeakTests, CleanupWeakPointer) {
  EFI_STATUS       Status;
  BORAX_ALLOCATOR  Alloc;

  BoraxAllocatorInit (&Alloc, Tracer.GetProtocol ());

  for (int i = 0; i < 1000; ++i) {
    BORAX_CONS *Cons;
    Status = BoraxAllocateCons(&Alloc, &Cons);
    EXPECT_EQ (EFI_SUCCESS, Status);

    BORAX_WEAK_POINTER *Wp;
    Status = BoraxAllocateWeakPointer(&Alloc, BORAX_MAKE_POINTER(Cons), &Wp);
    EXPECT_EQ (EFI_SUCCESS, Status);
  }

  BoraxAllocatorCleanup (&Alloc);
  ValidateReport ();
}

TEST_F (MemoryLeakTests, CollectNothing) {
  EFI_STATUS       Status;
  BORAX_ALLOCATOR  Alloc;

  BoraxAllocatorInit (&Alloc, Tracer.GetProtocol ());
  std::vector<BORAX_OBJECT> RootSet = {};
  auto RSI = MakeRootSetIterator(RootSet);
  Status = BoraxAllocatorCollect (&Alloc, RSI.Get());
  EXPECT_EQ (EFI_SUCCESS, Status);
  BoraxAllocatorCleanup (&Alloc);
  ValidateReport ();
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
