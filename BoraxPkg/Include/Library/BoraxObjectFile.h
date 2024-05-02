#ifndef BORAX_OBJECT_FILE_H
#define BORAX_OBJECT_FILE_H

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
 * The object file format is similar to ELF, but it is not compatible with ELF
 * and is heavily simplified. The Borax object file format is designated "BXO"
 * for brevity.
 *
 * The object file header
 * ======================
 *
 * The object file header is similar to an ELF file header: it marks the file
 * with a magic constant, it declares the target data model, and it points to
 * the section headers.
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
  UINT8    Magic[4]; // \x7F B X O
  UINT8    WordSize;
  UINT8    Endianness;
  UINT8    Version;
  UINT8    Pad; // 0x00
  UINTN    SectionHeaderOffset;
  UINTN    SectionHeaderCount;
} BXO_HEADER;

/*
 * File addresses
 * ==============
 *
 * Because the memory address(es) the object file will be loaded into are not
 * known in advance, object pointers are represented as file addresses. The low
 * 4 bits of a file address are ignored to accommodate lowtags. The next 12 bits
 * represent a byte offset within a page. The remaining high-order bits encode a
 * page index allocated according to the following scheme:
 *
 * The sections of an object file are loaded in the order in which their headers
 * appear in the section header array. A running index, starting at zero,
 * numbers pages as they are allocated. Indices 0 through N_0 - 1 are assigned
 * to the N_0 pages of section 0; indices N_0 through N_0 + N_1 - 1 are assigned
 * to the N_1 pages of section 1; and so on.
 *
 * For example: suppose an object file is loaded with three sections containing
 * 3, 5, and 7 pages respectively. These file addresses would decode as follows:
 *
 *   File address  Page index          Interpretation
 *   ------------  ----------  -----------------------------
 *     0x000004a0           0  Section 0, page 0, byte 0x4a0
 *     0x00002800           2  Section 0, page 2, byte 0x800
 *     0x00004000           4  Section 1, page 1, byte 0x000
 *     0x00008000           8  Section 2, page 0, byte 0x000
 *     0x0000eff0          14  Section 2, page 6, byte 0xff0
 *     0x0000f000          15  Out of bounds
 *
 * Not every byte offset within a page is a valid object address; byte offsets
 * are validated by the object file loader. This validation is dependent on the
 * kind of section pointed to.
 *
 * Sections
 * ========
 *
 * Sections are regions of page data in the object file. There are three types
 * of sections: cons sections, object sections, and relocation sections.  The
 * section headers declare the type and location of section data within an
 * object file. Cons, string, and relocation sections are handled uniformly, but
 * object sections are grouped by chunk size to simplify loading.
 *
 * Cons and object sections
 * ------------------------
 *
 * Cons and object sections contain page data that will be loaded into the
 * garbage collector. For simplicity, the file data contains space for garbage
 * collector metadata, but this data should be zeroed in the object file.
 *
 * Cons page byte offsets are validated with a simple alignment calculation. For
 * object pages, the first object is located at an offset of
 * BORAX_OBJECT_FIRST_INDEX from the beginning of the containing chunk, and each
 * subsequent object is stored at the first double-word-aligned offset following
 * its preceding object.
 *
 * String sections
 * ---------------
 *
 * String sections exist to resolve package and symbol relocations. Cons and
 * object sections may not reference strings.
 *
 * Strings are represented as NUL-terminated UCS-2 strings in native
 * endianness. The first string in a section starts at offset zero, and each
 * subsequent string immediately follows the preceding string's NUL terminator.
 *
 * The final code unit of a string section must be NUL.
 *
 * Package relocations
 * -------------------
 *
 * Package sections exist to resolve symbol relocations and references to
 * package objects. Cons and object sections may reference package relocations.
 *
 * Packages are represented as file addresses pointing to string data. To permit
 * references from tagged pointers, packages are aligned to 8-byte boundaries.
 *
 * Symbol relocations
 * ------------------
 *
 * Symbol sections exist to resolve both interned and uninterned symbols. Cons
 * and object sections may reference symbol relocations.
 *
 * Symbols are represented as a pair of words, the first being a file address
 * pointing to package data, and the second being a file address pointing to
 * string data. Uninterned symbols are represented by setting the package word
 * to the all-ones value.
 *
 * Class relocations
 * -----------------
 *
 * Class sections exist to resolve the Class field of record objects. Cons and
 * object sections may reference class relocations.
 *
 * Classes are represented as file addresses pointing to symbol data.
 */

typedef enum {
  BXO_SECTION_CONS        = 1,
  BXO_SECTION_OBJECT      = 2,
  BXO_SECTION_STRING      = 3,
  BXO_SECTION_REL_PACKAGE = 4,
  BXO_SECTION_REL_SYMBOL  = 5,
  BXO_SECTION_REL_CLASS   = 6,
} BXO_SECTION_TAG;

typedef struct {
  UINTN    Tag;
  UINTN    Address;
  UINTN    Size;
  UINTN    PagesPerChunk;  // Object pages only
} BXO_SECTION_HEADER;

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
 * 3. Pointers within the loaded sections are updated from file addresses to
 *    memory addresses.
 *
 * 4. Relocations are resolved to existing objects.
 *
 * 5. The cons and object sections are injected into the garbage collector.
 *
 * 6. Temporary memory is freed.
 *
 * Disk I/O is performed only in steps 1-2. Steps 4-5 must be performed
 * atomically with respect to garbage collection. To prevent disk I/O from
 * stalling the interpreter while loading object files, the process is split
 * into two phases: in the staging phase, steps 1-3 are performed
 * asynchronously; in the injection phase, steps 4-6 are performed
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
