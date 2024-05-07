#ifndef BORAX_OBJECT_FILE_H
#define BORAX_OBJECT_FILE_H

#include <Protocol/SimpleFileSystem.h>

#include <Library/BoraxMemory.h>

/*
 * Object files
 * ============
 *
 * Object files serve two purposes: representing the initial lisp image, and
 * representing compiled files. At a high level, an object file is a container
 * for a graph of lisp objects that are to be loaded into a running lisp
 * environment. The runtime maps the objects into memory, updates file addresses
 * to memory addresses, and returns a reference to designated "root" object.
 *
 * The graph of objects in an object file is not necessarily self-contained. A
 * record object, for instance, may need to reference a class object that is
 * defined outside the scope of the object file. To this end, object files also
 * contain relocation information.
 *
 * To facilitate fast and simple loading, the structure of an object file
 * reflects the memory structure of the garbage collector. Object files contain,
 * among other things, cons and object sections that directly store garbage
 * collector pages. This structure allows the runtime to use the loaded file
 * data directly without an intermediate copy, but it also makes object files
 * architecture-specific.
 *
 * The object file format is inspired by ELF, but it is not compatible with ELF
 * and it is heavily simplified. The Borax object file format is designated
 * "BXO" for brevity.
 *
 * The object file header
 * ======================
 *
 * The file header is the root structure of the object file. It marks the file
 * as an object file, declares the target data model, and contains the pointers
 * needed to interpret the rest of the file's structure. In contrast to ELF,
 * which supports an arbitrary mix of named sections, the sections of a BXO file
 * are fixed.
 */

enum {
  BXO_MAGIC0 = 0x7F,
  BXO_MAGIC1 = 'B',
  BXO_MAGIC2 = 'X',
  BXO_MAGIC3 = 'O',
};

enum {
  BXO_32BIT = 1,
  BXO_64BIT = 2,
};

enum {
  BXO_LITTLE_ENDIAN = 1,
  BXO_BIG_ENDIAN    = 2,
};

typedef struct {
  UINTN    Offset;
  UINTN    Size;
  UINTN    RelCount; // Relocation sections only
} BXO_SECTION;

typedef struct {
  UINT8          Magic[4]; // \x7F B X O
  UINT8          WordSize;
  UINT8          Endianness;
  UINT8          Version;
  UINT8          Pad; // 0x00
  UINTN          RootObject;
  BXO_SECTION    Cons;
  BXO_SECTION    Object;
  BXO_SECTION    String;
  BXO_SECTION    Package;
  BXO_SECTION    Symbol;
  BXO_SECTION    Class;
} BXO_HEADER;

/*
 * File addresses
 * ==============
 *
 * Because the memory address(es) the object file will be loaded into are not
 * known in advance, object pointers are represented as file addresses. The low
 * three bits of a file address are allocated for lowtags. The high three bits
 * indicate which section the address points to. The interpretation of the
 * remaining 26 or 58 "offset" bits is determined by the section tag.
 *
 *   32        29       3        0
 *   +---------+--------+--------+
 *   | Section | Offset | Lowtag |
 *   +---------+--------+--------+
 *
 * Sections
 * ========
 *
 * Sections are regions of page data in the object file. There are three types
 * of sections: cons sections, object sections, and relocation sections. The
 * file header declares the type and location of section data within an object
 * file.
 *
 * Cons and object sections
 * ------------------------
 *
 * The cons and object sections contain page data that will be loaded into the
 * garbage collector. Each section contains one allocator chunk, including
 * garbage collector metadata. This metadata should be zeroed in the object
 * file.
 *
 * File addresses pointing to cons and object sections are interpreted by
 * extending the offset bits with three zero-valued low-order bits, or
 * equivalently by masking out the lowtag as the runtime would do when
 * calculating an object address. The resulting eight-byte aligned offset is
 * treated as a byte offset into the relevant section.
 *
 * The loader validates that cons and object offsets point to valid objects. It
 * does this for cons offsets with a simple alignment check. The object section
 * is scanned for valid offsets as follows: the first object is located at an
 * offset of BORAX_OBJECT_FIRST_INDEX from the beginning of the section; each
 * subsequent object is stored at the first double-word-aligned offset following
 * its preceding object.
 *
 * String section
 * --------------
 *
 * The string section exists to resolve package and symbol relocations. The cons
 * and object sections may not reference strings.
 *
 * Strings are represented as NUL-terminated UCS-2 strings in native
 * endianness. The first string in a section starts at offset zero, and each
 * subsequent string immediately follows the preceding string's NUL
 * terminator. The final code unit of a string section must be NUL.
 *
 * When the string section is loaded, the strings are assigned indices starting
 * from zero. The offset of a file address pointing to the string section is
 * interpreted as such an index.
 *
 * Package relocations
 * -------------------
 *
 * The package section exists to resolve symbol relocations and references to
 * package objects. The cons and object sections may reference package
 * relocations.
 *
 * The package section contains a sequence of package references, each
 * consisting of one file address pointing to a string naming the
 * package. Packages are assigned indices starting from zero, and the offset of
 * a file address pointing to the package section refers to such an index.
 *
 * Symbol relocations
 * ------------------
 *
 * The symbol section exists to resolve both interned and uninterned
 * symbols. The cons and object sections may reference symbol relocations.
 *
 * The symbol section contains a seuqence of symbol references, each consisting
 * of two file addresses. The first file address points to the symbol's home
 * package, and the second points to a string naming the symbol. Uninterned
 * symbols are represented by setting the package word to the all-ones value.
 *
 * Class relocations
 * -----------------
 *
 * The class section exists to resolve the Class field of record objects. Cons
 * and object sections may reference class relocations.
 *
 * The class section is organized the same way as the package section, except
 * that the referenced string data names a class instead of a package.
 */

typedef enum {
  BXO_SECTION_CONS        = 1,
  BXO_SECTION_OBJECT      = 2,
  BXO_SECTION_STRING      = 3,
  BXO_SECTION_REL_PACKAGE = 4,
  BXO_SECTION_REL_SYMBOL  = 5,
  BXO_SECTION_REL_CLASS   = 6,
} BXO_SECTION_TAG;

/*
 * Loading object files
 * ====================
 *
 * Object files are loaded according to the following procedure:
 *
 * 1. The object file headers are read from disk and used to allocate memory
 *    needed for step 2.
 *
 * 2. Sections are loaded into the memory allocated in step 1.
 *
 * 3. Relocations are resolved to existing objects.
 *
 * 4. Pointers within the loaded sections are updated from file addresses to
 *    memory addresses.
 *
 * 5. The cons and object sections are injected into the garbage collector.
 *
 * 6. Temporary memory is freed.
 *
 * Disk I/O is performed only in steps 1-2. Steps 3-5 must be performed
 * atomically with respect to garbage collection. To prevent disk I/O from
 * stalling the interpreter while loading object files, the process is split
 * into two phases: in the staging phase, steps 1-2 are performed
 * asynchronously; in the injection phase, steps 3-6 are performed
 * synchronously.
 *
 * The staging phase is initiated with BoraxStageObjectFile and ends when the
 * Complete event is awaited. The caller is responsible for initializing and
 * destroying the Complete event. The staging phase may be cancelled with
 * BoraxCancelObjectFile; the Complete event must be awaited as usual. If the
 * staging phase completes successfully, the staged object file must either be
 * injected with BoraxInjectObjectFile or discarded with BoraxUnStageObjectFile.
 *
 * A synchronous version of the entire process is provided for convenience.
 */

typedef struct _BORAX_STAGED_OBJECT_FILE_IMPL BORAX_STAGED_OBJECT_FILE_IMPL;

typedef struct {
  EFI_EVENT                        Complete;
  EFI_STATUS                       Status;
  BORAX_STAGED_OBJECT_FILE_IMPL    *Impl;
} BORAX_STAGED_OBJECT_FILE;

VOID
EFIAPI
BoraxStageObjectFile (
  IN BORAX_ALLOCATOR            *Alloc,
  IN EFI_FILE_PROTOCOL          *File,
  OUT BORAX_STAGED_OBJECT_FILE  *Staged
  );

VOID
EFIAPI
BoraxCancelObjectFile (
  IN BORAX_STAGED_OBJECT_FILE  *Staged
  );

VOID
EFIAPI
BoraxUnStageObjectFile (
  IN BORAX_STAGED_OBJECT_FILE  *Staged
  );

EFI_STATUS
EFIAPI
BoraxInjectObjectFile (
  IN BORAX_STAGED_OBJECT_FILE  *Staged,
  OUT BORAX_PIN                **Pin
  );

EFI_STATUS
EFIAPI
BoraxLoadObjectFile (
  IN BORAX_ALLOCATOR    *Alloc,
  IN EFI_FILE_PROTOCOL  *File,
  OUT BORAX_PIN         **Pin
  );

#endif // BORAX_OBJECT_FILE_H
