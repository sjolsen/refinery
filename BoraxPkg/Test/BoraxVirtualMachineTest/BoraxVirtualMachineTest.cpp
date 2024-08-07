#include <filesystem>
#include <fstream>
#include <optional>
#include <vector>

#include <gmock/gmock.h>
#include <gtest/gtest.h>

extern "C" {
  #include <Uefi.h>
  #include <Library/UefiBootServicesTableLib.h>
  #include <Library/BoraxMemory.h>
  #include <Library/BoraxObjectFile.h>
}

#include "MockEvent.hpp"
#include "MockFile.hpp"
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

  EXPECT_THAT (Conses1, Each (IsValidAddress (&Tracer)));
  Collect ();
  auto  Conses2 = MakeConses (1000);

  EXPECT_THAT (Conses1, Each (Not (IsValidAddress (&Tracer))));
  EXPECT_THAT (Conses2, Each (IsValidAddress (&Tracer)));
}

TEST_F (MemoryTests, RootedCons) {
  BORAX_CONS  *Cons = MakeCons ();
  AutoPin     Pin   = MakePin (Cons);

  Cons->Car = 42 << 1;  // fixnums
  Cons->Cdr = 77 << 1;

  Collect ();

  ASSERT_THAT (Pin.get (), IsValidAddress (&Tracer));
  ASSERT_TRUE (BORAX_IS_POINTER (Pin->Object));
  BORAX_OBJECT_HEADER  *Header = BORAX_GET_POINTER (Pin->Object);

  ASSERT_THAT (Header, IsValidAddress (&Tracer));
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

  ASSERT_THAT (Pin.get (), IsValidAddress (&Tracer));
  ASSERT_TRUE (BORAX_IS_POINTER (Pin->Object));
  BORAX_OBJECT_HEADER  *Header = BORAX_GET_POINTER (Pin->Object);

  ASSERT_THAT (Header, IsValidAddress (&Tracer));
  ASSERT_TRUE (BORAX_IS_CONS (Header));
  BORAX_CONS  *P = reinterpret_cast<BORAX_CONS *>(Header);

  for (size_t i = 0; i < Conses.size (); ++i) {
    EXPECT_EQ (i << 1, P->Car);
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

  EXPECT_EQ ((UINTN)(343 << 1), P->Car);
  EXPECT_EQ ((UINTN)(2401 << 1), P->Cdr);
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

class ObjectValidationError : public std::exception {
};

class NotValidAddressError : public ObjectValidationError {
public:
  const char *
  what (
    ) const noexcept override
  {
    return "invalid address";
  }
};

class TypeError : public ObjectValidationError {
private:
  std::string WhatStr;

  const char *
  GetDiscriminatorString (
    BORAX_OBJECT  Object
    )
  {
    struct DiscrimEntry {
      UINTN         Value;
      const char    *Name;
    };

    static constexpr const DiscrimEntry  Entries[] = {
      { BORAX_DISCRIM_FIXNUM,        "FIXNUM"        },
      { BORAX_DISCRIM_UNBOUND,       "UNBOUND"       },
      { BORAX_DISCRIM_CHARACTER,     "CHARACTER"     },
      { BORAX_DISCRIM_CONS,          "CONS"          },
      { BORAX_DISCRIM_WORD_RECORD,   "WORD_RECORD"   },
      { BORAX_DISCRIM_OBJECT_RECORD, "OBJECT_RECORD" },
      { BORAX_DISCRIM_WEAK_POINTER,  "WEAK_POINTER"  },
      { BORAX_DISCRIM_PIN,           "PIN"           },
      { BORAX_DISCRIM_MOVED,         "MOVED"         },
      { BORAX_DISCRIM_UNINITIALIZED, "UNINITIALIZED" },
      { 0,                           nullptr         }
    };

    UINTN  Discrim = BORAX_DISCRIMINATE (Object);

    for (std::size_t i = 0; Entries[i].Name != nullptr; ++i) {
      if (Entries[i].Value == Discrim) {
        return Entries[i].Name;
      }
    }

    return "(unknown)";
  }

public:
  TypeError(
            std::string_view  Expected,
            BORAX_OBJECT      Actual
            )
  {
    std::stringstream  ss;

    ss << "type error: expected " << Expected
    << "; got " << GetDiscriminatorString (Actual);
    WhatStr = std::move (ss).str ();
  }

  const char *
  what (
    ) const noexcept override
  {
    return WhatStr.c_str ();
  }
};

class ObjectFileTests : public MemoryTests {
public:
  static std::filesystem::path TestFilePath;
  MockEventEngine EventEngine;

  EFI_STATUS
  LoadObjectFile (
    MockFile  &File,
    AutoPin   *Pin
    )
  {
    EFI_STATUS  Status;
    BORAX_PIN   *RawPin;

    Status = BoraxLoadObjectFile (&Alloc, File.GetProtocol (), &RawPin);
    if (Status == EFI_SUCCESS) {
      *Pin = AutoPin { RawPin, PinDeleter () };
    }

    return Status;
  }

  template <typename T>
  T *
  TheValidAddress (
    T  *Pointer
    )
  {
    if (!Tracer.IsValidAddress (Pointer)) {
      throw NotValidAddressError ();
    }

    return Pointer;
  }

  BORAX_OBJECT
  ThePinnedObject (
    const AutoPin  &Pin
    )
  {
    return TheValidAddress (Pin.get ())->Object;
  }

  BORAX_OBJECT_HEADER *
  TheObject (
    BORAX_OBJECT  Object
    )
  {
    if (!BORAX_IS_POINTER (Object)) {
      throw TypeError ("object", Object);
    }

    return TheValidAddress (BORAX_GET_POINTER (Object));
  }

  BORAX_CONS *
  TheCons (
    BORAX_OBJECT  Object
    )
  {
    BORAX_OBJECT_HEADER  *Header = TheObject (Object);

    if (!BORAX_IS_CONS (Header)) {
      throw TypeError ("cons", Object);
    }

    return reinterpret_cast<BORAX_CONS *>(Header);
  }

  BORAX_RECORD *
  TheWordRecord (
    BORAX_OBJECT  Object
    )
  {
    BORAX_OBJECT_HEADER  *Header = TheObject (Object);

    if (Header->WideTag != BORAX_WIDETAG_WORD_RECORD) {
      throw TypeError ("word-record", Object);
    }

    return reinterpret_cast<BORAX_RECORD *>(Header);
  }

  BORAX_RECORD *
  TheObjectRecord (
    BORAX_OBJECT  Object
    )
  {
    BORAX_OBJECT_HEADER  *Header = TheObject (Object);

    if (Header->WideTag != BORAX_WIDETAG_OBJECT_RECORD) {
      throw TypeError ("object-record", Object);
    }

    return reinterpret_cast<BORAX_RECORD *>(Header);
  }

  VOID
  CheckGeneratedFileContents (
    AutoPin  &Pin
    );
};

std::filesystem::path  ObjectFileTests::TestFilePath = { };

TEST_F (ObjectFileTests, EmptyFile) {
  EFI_STATUS  Status;
  AutoPin     Pin;
  BufferFile  File {
    { }
  };

  Status = LoadObjectFile (File, &Pin);
  ASSERT_EQ (EFI_END_OF_FILE, Status);
}

TEST_F (ObjectFileTests, ZeroHeader) {
  EFI_STATUS  Status;
  AutoPin     Pin;
  BufferFile  File { std::vector<unsigned char>(sizeof (BXO_HEADER), 0) };

  Status = LoadObjectFile (File, &Pin);
  ASSERT_EQ (EFI_LOAD_ERROR, Status);
}

static constexpr const unsigned char  HeaderOnly32[] = {
  // BXO file
  0x7f, 'B', 'X', 'O',
  // 32-bit, version 0
  1,    0,   0,   0,
  // Root object (fixnum 0)
  0,    0,   0,   0,
  // Cons section (no data)
  0,    0,   0,   0,
  0,    0,   0,   0,
  0,    0,   0,   0,
  // Object section (no data)
  0,    0,   0,   0,
  0,    0,   0,   0,
  0,    0,   0,   0,
  // String section (no data)
  0,    0,   0,   0,
  0,    0,   0,   0,
  0,    0,   0,   0,
  // Package section (no data)
  0,    0,   0,   0,

  0,    0,   0,   0,
  0,    0,   0,   0,
  // Symbol section (no data)
  0,    0,   0,   0,
  0,    0,   0,   0,
  0,    0,   0,   0,
  // Class section (no data)
  0,    0,   0,   0,
  0,    0,   0,   0,
  0,    0,   0,   0,
};

static constexpr const unsigned char  HeaderOnly64[] = {
  // BXO file
  0x7f, 'B', 'X', 'O',
  // 64-bit, version 0
  2,    0,   0,   0,
  // Root object (fixnum 0)
  0,    0,   0,   0,  0,  0, 0, 0,
  // Cons section (no data)
  0,    0,   0,   0,  0,  0, 0, 0,
  0,    0,   0,   0,  0,  0, 0, 0,
  0,    0,   0,   0,  0,  0, 0, 0,
  // Object section (no data)
  0,    0,   0,   0,  0,  0, 0, 0,
  0,    0,   0,   0,  0,  0, 0, 0,
  0,    0,   0,   0,  0,  0, 0, 0,
  // String section (no data)
  0,    0,   0,   0,  0,  0, 0, 0,
  0,    0,   0,   0,  0,  0, 0, 0,
  0,    0,   0,   0,  0,  0, 0, 0,
  // Package section (no data)
  0,    0,   0,   0,  0,  0, 0, 0,
  0,    0,   0,   0,  0,  0, 0, 0,
  0,    0,   0,   0,  0,  0, 0, 0,
  // Symbol section (no data)
  0,    0,   0,   0,  0,  0, 0, 0,
  0,    0,   0,   0,  0,  0, 0, 0,
  0,    0,   0,   0,  0,  0, 0, 0,
  // Class section (no data)
  0,    0,   0,   0,  0,  0, 0, 0,
  0,    0,   0,   0,  0,  0, 0, 0,
  0,    0,   0,   0,  0,  0, 0, 0,
};

// TODO: Come up with a better way of setting word size
#if defined (MDE_CPU_IA32)
#define HEADER_ONLY_NATIVE      HeaderOnly32
#define HEADER_ONLY_NON_NATIVE  HeaderOnly64
#elif defined (MDE_CPU_X64)
#define HEADER_ONLY_NATIVE      HeaderOnly64
#define HEADER_ONLY_NON_NATIVE  HeaderOnly32
#else
  #error "Don't know what memory model to use"
#endif

template <std::size_t N>
std::vector<unsigned char>
MakeVector (
  const unsigned char (&Data)[N]
  )
{
  return { Data, Data + N };
}

TEST_F (ObjectFileTests, HeaderOnlyNative) {
  EFI_STATUS  Status;
  AutoPin     Pin;
  BufferFile  File { MakeVector (HEADER_ONLY_NATIVE) };

  Status = LoadObjectFile (File, &Pin);
  ASSERT_EQ (EFI_SUCCESS, Status);
  EXPECT_EQ (static_cast<BORAX_OBJECT>(0), Pin->Object);
}

TEST_F (ObjectFileTests, HeaderOnlyNonNative) {
  EFI_STATUS  Status;
  AutoPin     Pin;
  BufferFile  File { MakeVector (HEADER_ONLY_NON_NATIVE) };

  // Loading fails for different reasons depending on the architecture: 32-bit
  // code can't load 64-bit object files because the identification data in the
  // header doesn't match, but 64-bit code can't even load the identification
  // data because the file is too small.
  Status = LoadObjectFile (File, &Pin);
  ASSERT_NE (EFI_SUCCESS, Status);
}

static
std::optional<std::vector<unsigned char> >
LoadFile (
  std::filesystem::path  Path
  )
{
  std::ifstream  Stream (Path, std::ios::binary);

  if (Stream.fail ()) {
    std::cerr << "Failed to construct stream for " << Path << std::endl;
    return std::nullopt;
  }

  std::vector<unsigned char>  Result;

  Stream.unsetf (std::ios::skipws);
  std::copy (
         std::istream_iterator<unsigned char>(Stream),
         std::istream_iterator<unsigned char>(),
         std::back_inserter (Result)
         );
  if (!Stream.eof ()) {
    std::cerr << "Failed to read " << Path << std::endl;
    return std::nullopt;
  }

  return std::move (Result);
}

VOID
ObjectFileTests::CheckGeneratedFileContents (
  AutoPin  &Pin
  )
{
  // Check the root object's class
  BORAX_RECORD  *Root;
  BORAX_RECORD  *RootClass;
  BORAX_RECORD  *RootClassClass;

  ASSERT_NO_THROW (Root           = TheObjectRecord (ThePinnedObject (Pin)));
  ASSERT_NO_THROW (RootClass      = TheObjectRecord (Root->Class));
  ASSERT_NO_THROW (RootClassClass = TheObjectRecord (RootClass->Class));
  ASSERT_EQ (RootClass, RootClassClass);
  ASSERT_EQ (0U, RootClass->Length);

  // root[0] is a circular reference back to the root
  BORAX_RECORD  *RootSelf;

  ASSERT_EQ (6U, Root->Length);
  ASSERT_NO_THROW (RootSelf = TheObjectRecord (Root->Data[0]));
  ASSERT_EQ (Root, RootSelf);

  // root[1] is an improper list (4 3 2 1 . 0)
  BORAX_CONS  *Cons[8];

  ASSERT_NO_THROW (Cons[0] = TheCons (Root->Data[1]));
  ASSERT_EQ (4U << 1, Cons[0]->Car);
  ASSERT_NO_THROW (Cons[1] = TheCons (Cons[0]->Cdr));
  ASSERT_EQ (3U << 1, Cons[1]->Car);
  ASSERT_NO_THROW (Cons[2] = TheCons (Cons[1]->Cdr));
  ASSERT_EQ (2U << 1, Cons[2]->Car);
  ASSERT_NO_THROW (Cons[3] = TheCons (Cons[2]->Cdr));
  ASSERT_EQ (1U << 1, Cons[3]->Car);
  ASSERT_EQ (0U << 1, Cons[3]->Cdr);

  // root[2] is a circular list #1=(8 7 6 5 . #1#)
  ASSERT_NO_THROW (Cons[4] = TheCons (Root->Data[2]));
  ASSERT_EQ (8U << 1, Cons[4]->Car);
  ASSERT_NO_THROW (Cons[5] = TheCons (Cons[4]->Cdr));
  ASSERT_EQ (7U << 1, Cons[5]->Car);
  ASSERT_NO_THROW (Cons[6] = TheCons (Cons[5]->Cdr));
  ASSERT_EQ (6U << 1, Cons[6]->Car);
  ASSERT_NO_THROW (Cons[7] = TheCons (Cons[6]->Cdr));
  ASSERT_EQ (5U << 1, Cons[7]->Car);
  ASSERT_EQ (Cons[4], TheCons (Cons[7]->Cdr));

  // root[3] is an object vector containing fixnums
  BORAX_RECORD  *ObjectVector;
  BORAX_RECORD  *ObjectVectorClass;

  ASSERT_NO_THROW (ObjectVector      = TheObjectRecord (Root->Data[3]));
  ASSERT_NO_THROW (ObjectVectorClass = TheObjectRecord (ObjectVector->Class));
  ASSERT_EQ (RootClass, ObjectVectorClass);
  ASSERT_EQ (3U, ObjectVector->Length);
  ASSERT_EQ (343U << 1, ObjectVector->Data[0]);
  ASSERT_EQ (8675309U << 1, ObjectVector->Data[1]);
  ASSERT_EQ (
    static_cast<UINTN>(static_cast<INTN>(-9000) << 1),
    ObjectVector->Data[2]
    );

  // root[4] is a word vector
  BORAX_RECORD  *WordVector;
  BORAX_RECORD  *WordVectorClass;

  ASSERT_NO_THROW (WordVector      = TheWordRecord (Root->Data[4]));
  ASSERT_NO_THROW (WordVectorClass = TheObjectRecord (WordVector->Class));
  ASSERT_EQ (RootClass, WordVectorClass);
  ASSERT_EQ (3U, WordVector->Length);
  ASSERT_EQ (343U, WordVector->Data[0]);
  ASSERT_EQ (8675309U, WordVector->Data[1]);
  ASSERT_EQ (static_cast<UINTN>(-9000), WordVector->Data[2]);

  // root[5] is a byte vector #(0 1 2)
  BORAX_RECORD  *ByteVector;
  BORAX_RECORD  *ByteVectorClass;
  UINT8         *ByteVectorData;

  ASSERT_NO_THROW (ByteVector      = TheWordRecord (Root->Data[5]));
  ASSERT_NO_THROW (ByteVectorClass = TheObjectRecord (ByteVector->Class));
  ASSERT_EQ (RootClass, ByteVectorClass);
  ASSERT_EQ (sizeof (UINTN), 3U + ByteVector->LengthAux);
  ASSERT_EQ (1U, ByteVector->Length);
  ByteVectorData = reinterpret_cast<UINT8 *>(ByteVector->Data);
  ASSERT_EQ (0U, ByteVectorData[0]);
  ASSERT_EQ (1U, ByteVectorData[1]);
  ASSERT_EQ (2U, ByteVectorData[2]);
}

TEST_F (ObjectFileTests, GeneratedTestFile) {
  EFI_STATUS  Status;
  AutoPin     Pin;

  ASSERT_FALSE (TestFilePath.empty ());
  auto  Data = LoadFile (TestFilePath);

  ASSERT_TRUE (Data.has_value ());
  BufferFile  File { std::move (*Data) };

  Status = LoadObjectFile (File, &Pin);
  ASSERT_EQ (EFI_SUCCESS, Status);

  CheckGeneratedFileContents (Pin);
  Collect ();
  CheckGeneratedFileContents (Pin);
}

int
main (
  int   argc,
  char  *argv[]
  )
{
  testing::InitGoogleTest (&argc, argv);
  if (argc >= 2) {
    ObjectFileTests::TestFilePath = argv[1];
  }

  return RUN_ALL_TESTS ();
}
