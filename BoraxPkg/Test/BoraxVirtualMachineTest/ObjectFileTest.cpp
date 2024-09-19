#include "MemoryTest.hpp"

extern "C" {
  #include <Library/BoraxObjectFile.h>
}

#include "BoraxVirtualMachineTest.hpp"
#include "MockEvent.hpp"
#include "MockFile.hpp"

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
  EXPECT_EQ (BORAX_MAKE_FIXNUM (0), Pin->Object);
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
  ASSERT_NO_THROW (RootSelf = TheObjectRecord (Root->Slots[0]));
  ASSERT_EQ (Root, RootSelf);

  // root[1] is an improper list (4 3 2 1 . 0)
  BORAX_CONS  *Cons[8];

  ASSERT_NO_THROW (Cons[0] = TheCons (Root->Slots[1]));
  ASSERT_EQ (BORAX_MAKE_FIXNUM (4), Cons[0]->Car);
  ASSERT_NO_THROW (Cons[1] = TheCons (Cons[0]->Cdr));
  ASSERT_EQ (BORAX_MAKE_FIXNUM (3), Cons[1]->Car);
  ASSERT_NO_THROW (Cons[2] = TheCons (Cons[1]->Cdr));
  ASSERT_EQ (BORAX_MAKE_FIXNUM (2), Cons[2]->Car);
  ASSERT_NO_THROW (Cons[3] = TheCons (Cons[2]->Cdr));
  ASSERT_EQ (BORAX_MAKE_FIXNUM (1), Cons[3]->Car);
  ASSERT_EQ (BORAX_MAKE_FIXNUM (0), Cons[3]->Cdr);

  // root[2] is a circular list #1=(8 7 6 5 . #1#)
  ASSERT_NO_THROW (Cons[4] = TheCons (Root->Slots[2]));
  ASSERT_EQ (BORAX_MAKE_FIXNUM (8), Cons[4]->Car);
  ASSERT_NO_THROW (Cons[5] = TheCons (Cons[4]->Cdr));
  ASSERT_EQ (BORAX_MAKE_FIXNUM (7), Cons[5]->Car);
  ASSERT_NO_THROW (Cons[6] = TheCons (Cons[5]->Cdr));
  ASSERT_EQ (BORAX_MAKE_FIXNUM (6), Cons[6]->Car);
  ASSERT_NO_THROW (Cons[7] = TheCons (Cons[6]->Cdr));
  ASSERT_EQ (BORAX_MAKE_FIXNUM (5), Cons[7]->Car);
  ASSERT_EQ (Cons[4], TheCons (Cons[7]->Cdr));

  // root[3] is an object vector containing fixnums
  BORAX_RECORD  *ObjectVector;
  BORAX_RECORD  *ObjectVectorClass;

  ASSERT_NO_THROW (ObjectVector      = TheObjectRecord (Root->Slots[3]));
  ASSERT_NO_THROW (ObjectVectorClass = TheObjectRecord (ObjectVector->Class));
  ASSERT_EQ (RootClass, ObjectVectorClass);
  ASSERT_EQ (3U, ObjectVector->Length);
  ASSERT_EQ (BORAX_MAKE_FIXNUM (343), ObjectVector->Slots[0]);
  ASSERT_EQ (BORAX_MAKE_FIXNUM (8675309), ObjectVector->Slots[1]);
  ASSERT_EQ (BORAX_MAKE_FIXNUM (-9000), ObjectVector->Slots[2]);

  // root[4] is a word vector
  BORAX_RECORD  *WordVector;
  BORAX_RECORD  *WordVectorClass;

  ASSERT_NO_THROW (WordVector      = TheWordRecord (Root->Slots[4]));
  ASSERT_NO_THROW (WordVectorClass = TheObjectRecord (WordVector->VectorClass));
  ASSERT_EQ (RootClass, WordVectorClass);
  ASSERT_EQ (3U, WordVector->Length);
  ASSERT_EQ (343U, WordVector->Data[0]);
  ASSERT_EQ (8675309U, WordVector->Data[1]);
  ASSERT_EQ (static_cast<UINTN>(-9000), WordVector->Data[2]);

  // root[5] is a byte vector #(0 1 2)
  BORAX_RECORD  *ByteVector;
  BORAX_RECORD  *ByteVectorClass;
  UINT8         *ByteVectorData;

  ASSERT_NO_THROW (ByteVector      = TheWordRecord (Root->Slots[5]));
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
  PosixFile   File { TestBase / "TestFile.bxo" };

  Status = LoadObjectFile (File, &Pin);
  ASSERT_EQ (EFI_SUCCESS, Status);

  CheckGeneratedFileContents (Pin);
  Collect ();
  CheckGeneratedFileContents (Pin);
}
