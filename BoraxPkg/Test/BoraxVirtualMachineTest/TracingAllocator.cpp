#include "TracingAllocator.hpp"

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
