#ifndef BORAX_VIRTUAL_MACHINE_TEST_HPP
#define BORAX_VIRTUAL_MACHINE_TEST_HPP

#include <filesystem>

extern "C" {
  #include <Library/BoraxMemory.h>
}

static inline bool
operator== (
  BORAX_OBJECT  a,
  BORAX_OBJECT  b
  )
{
  return BORAX_EQ (a, b);
}

extern std::filesystem::path  TestFilePath;

#endif // BORAX_VIRTUAL_MACHINE_TEST_HPP
