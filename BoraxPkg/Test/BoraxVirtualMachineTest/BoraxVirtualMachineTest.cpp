#include "BoraxVirtualMachineTest.hpp"

#include <gtest/gtest.h>

std::filesystem::path  TestBase = { };

int
main (
  int   argc,
  char  *argv[]
  )
{
  testing::InitGoogleTest (&argc, argv);
  if (argc >= 2) {
    TestBase = argv[1];
  }

  return RUN_ALL_TESTS ();
}
