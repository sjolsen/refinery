#include "BoraxVirtualMachineTest.hpp"

#include <gtest/gtest.h>

std::filesystem::path  TestFilePath = { };

int
main (
  int   argc,
  char  *argv[]
  )
{
  testing::InitGoogleTest (&argc, argv);
  if (argc >= 2) {
    TestFilePath = argv[1];
  }

  return RUN_ALL_TESTS ();
}
