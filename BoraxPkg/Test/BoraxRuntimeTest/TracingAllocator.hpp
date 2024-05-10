#ifndef BORAX_TRACING_ALLOCATOR_H
#define BORAX_TRACING_ALLOCATOR_H

#include <unordered_map>

extern "C" {
  #include <Library/BaseLib.h>
  #include <Library/BoraxMemory.h>
}

#include "WrapFn.hpp"

class TracingAllocator
  : public ProtocolClass<TracingAllocator, BORAX_SYSTEM_ALLOCATOR_PROTOCOL> {
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
  std::vector<std::shared_ptr<void> > _Allocs;
  std::unordered_map<UINTN, PageAllocation> _PageAllocs;
  std::unordered_map<UINTN, PoolAllocation> _PoolAllocs;
  std::vector<std::string> _Errors;

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

    _Allocs.emplace_back (Mem, free);
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
    }
  }

  VOID *
  _AllocatePool (
    UINTN  AllocationSize
    )
  {
    VOID            *Mem   = malloc (AllocationSize);
    PoolAllocation  Record = { Mem, AllocationSize };

    _Allocs.emplace_back (Mem, free);
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
    }
  }

public:
  TracingAllocator(
                   )
  {
    BORAX_SYSTEM_ALLOCATOR_PROTOCOL  *Protocol = GetProtocol ();

    Protocol->AllocatePages = WRAP_FN (_AllocatePages);
    Protocol->FreePages     = WRAP_FN (_FreePages);
    Protocol->AllocatePool  = WRAP_FN (_AllocatePool);
    Protocol->FreePool      = WRAP_FN (_FreePool);
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
  return Alloc->IsValidAddress (arg);
}

#endif // BORAX_TRACING_ALLOCATOR_H
