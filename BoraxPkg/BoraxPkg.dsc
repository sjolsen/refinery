[Defines]
  DSC_SPECIFICATION       = 1.28
  PLATFORM_NAME           = Borax
  PLATFORM_GUID           = 1eba589a-305c-477b-9cfe-6c5d092bc3e6
  PLATFORM_VERSION        = 0.00
  SKUID_IDENTIFIER        = DEFAULT
  SUPPORTED_ARCHITECTURES = IA32|X64
  BUILD_TARGETS           = DEBUG|RELEASE|NOOPT

!include UnitTestFrameworkPkg/UnitTestFrameworkPkgHost.dsc.inc

[LibraryClasses]
  BoraxVirtualMachine|BoraxPkg/Library/BoraxVirtualMachine/BoraxVirtualMachine.inf
  SafeIntLib|MdePkg/Library/BaseSafeIntLib/BaseSafeIntLib.inf

[Components]
  BoraxPkg/Test/BoraxVirtualMachineTest/BoraxVirtualMachineTest.inf
