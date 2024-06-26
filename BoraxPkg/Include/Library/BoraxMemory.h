#ifndef BORAX_MEMORY_H
#define BORAX_MEMORY_H

#include <limits.h>

#include <Protocol/BoraxSystemAllocator.h>

/*
 * Object representation
 * =====================
 *
 * Lisp objects are partitioned into two kinds: "immediate objects," which are
 * small enough to fit into a machine word, and "heap objects," which exceed the
 * size of a machine word and are therefore always allocated on the heap. Heap
 * objects are further subdivided into cons cells, which are optimized to occupy
 * exactly two words on the heap, and "proper objects," which begin with a
 * header. Heap locations may also contain special markers which are not,
 * strictly speaking, objects.
 *
 * The storage scheme is patterned after SBCL's object representation. Objects
 * are tagged in the low-order bits, and the value of the tag together with the
 * memory location the word was loaded from identify the type of object:
 *
 * - In a word loaded from heap address A, an immediate-class tag indicates that
 *   the object at address A is a cons cell, and the word we've loaded is the
 *   cell's CAR.
 *
 * - In a word loaded from heap address A, a heap-class tag indicates that the
 *   object at address A is a proper object or that there is no object at
 *   address A.
 *
 * - In a word loaded from a non-heap location, an immediate-class tag marks the
 *   immediate object as a scalar value or a pointer to some heap object.
 *
 * - The remaining case, a heap-class tag in a word loaded from a non-heap
 *   location, is not a legal representation.
 *
 * Following SBCL, we reserve a least-significant bit of zero to mark
 * fixnums. This simplifies arithmetic and maximizes the encoding space
 * available for numeric data. All other tags must therefore have an LSB of
 * one. The tags are allocated as follows:
 *
 *   Tag  Class      Interpretation
 *   ===  =========  ==============
 *   xx0  immediate  fixnum
 *   001  immediate  pointer
 *   101  immediate  other
 *   x11  heap       object
 *
 * Note that the use of three least-significant bits for tagging implies an
 * object alignment of at least eight bytes. This is stricter than the natural
 * alignment of a machine word on a 32-bit system and the same on a 64-bit
 * system. In any case, this is sufficient to address lisp objects, which occupy
 * a minimum of two machine words.
 *
 * Again following SBCL, heap locations matching the basic tag listed above are
 * discriminated by expanding the tag to eight bits:
 *
 *   Tag   Interpretation
 *   ====  ==============
 *   0x03  word-record
 *   0x07  object-record
 *   ...
 *   0xF3  weak-pointer
 *   0xF7  pin
 *   0xFB  moved
 *   0xFF  uninitialized
 *
 * Here, 0xFF has been chosen as the uninitialized marker to allow initializing
 * raw memory with memset (the C idiom is to initialize with 0x00, but we've
 * already assigned that encoding to fixnums).
 *
 * TODO: Remainder of heap object representation.
 */

STATIC_ASSERT (
  sizeof (UINTN) == sizeof (VOID *),
  "UINTN expected to be the same size as VOID *"
  );

#if defined (MDE_CPU_IA32)
#define BORAX_HALFWORD      UINT16
#define BORAX_HALFWORD_MAX  MAX_UINT16
#elif defined (MDE_CPU_X64)
#define BORAX_HALFWORD      UINT32
#define BORAX_HALFWORD_MAX  MAX_UINT32
#else
  #error "Don't know how large to make a halfword"
#endif

typedef UINTN BORAX_OBJECT;

typedef enum {
  BORAX_LOWTAG_FIXNUM          = 0,
  BORAX_LOWTAG_POINTER         = 1,
  BORAX_LOWTAG_OTHER_IMMEDIATE = 5,
  BORAX_LOWTAG_HEAP            = 3,
} BORAX_LOWTAG;

enum {
  BORAX_LOWTAG_MASK_FIXNUM          = 1,
  BORAX_LOWTAG_MASK_POINTER         = 7,
  BORAX_LOWTAG_MASK_OTHER_IMMEDIATE = 7,
  BORAX_LOWTAG_MASK_HEAP            = 3,
};

enum {
  BORAX_IMMEDIATE_UNBOUND         = 0x00 | BORAX_LOWTAG_OTHER_IMMEDIATE,
  BORAX_IMMEDIATE_CHARACTER_BEGIN = 0x000007F8 | BORAX_LOWTAG_OTHER_IMMEDIATE,
  BORAX_IMMEDIATE_CHARACTER_END   = 0xFFFFFFF8 | BORAX_LOWTAG_OTHER_IMMEDIATE,
};

enum {
  BORAX_IMMEDIATE_MASK_CHARACTER = 0x7FF  // 21 bits for Unicode
};

#define BORAX_IS_FIXNUM(_obj) \
(((_obj) & BORAX_LOWTAG_MASK_FIXNUM) == BORAX_LOWTAG_FIXNUM)

#define BORAX_MAKE_FIXNUM(_n) \
((_n) << 1)

#define BORAX_IS_CHARACTER(_obj) \
(((_obj) & BORAX_IMMEDIATE_MASK_CHARACTER) == BORAX_IMMEDIATE_CHARACTER_BEGIN)

#define BORAX_IS_POINTER(_obj) \
(((_obj) & BORAX_LOWTAG_MASK_POINTER) == BORAX_LOWTAG_POINTER)

#define BORAX_GET_POINTER(_obj) \
((BORAX_OBJECT_HEADER *)((_obj) & ~BORAX_LOWTAG_MASK_POINTER))

#define BORAX_MAKE_POINTER(_ptr) \
((UINTN)(_ptr) | BORAX_LOWTAG_POINTER)

#define BORAX_IS_CONS(_ptr) \
(((_ptr)->HeaderWords[0] & BORAX_LOWTAG_MASK_HEAP) != BORAX_LOWTAG_HEAP)

enum {
  BORAX_WIDETAG_WORD_RECORD   = 0x03,
  BORAX_WIDETAG_OBJECT_RECORD = 0x07,
  BORAX_WIDETAG_WEAK_POINTER  = 0xF3,
  BORAX_WIDETAG_PIN           = 0xF7,
  BORAX_WIDETAG_MOVED         = 0xFB,
  BORAX_WIDETAG_UNINITIALIZED = 0xFF,
};

// Helper enum for simplifying switch statements. Values are implementation
// details; use BORAX_DISCRIMINATE.
enum {
  // Immediates
  BORAX_DISCRIM_FIXNUM    = 0x00,
  BORAX_DISCRIM_UNBOUND   = BORAX_IMMEDIATE_UNBOUND,
  BORAX_DISCRIM_CHARACTER = BORAX_IMMEDIATE_CHARACTER_BEGIN,
  // Pointers
  BORAX_DISCRIM_CONS          = 0x02,
  BORAX_DISCRIM_WORD_RECORD   = BORAX_WIDETAG_WORD_RECORD,
  BORAX_DISCRIM_OBJECT_RECORD = BORAX_WIDETAG_OBJECT_RECORD,
  BORAX_DISCRIM_WEAK_POINTER  = BORAX_WIDETAG_WEAK_POINTER,
  BORAX_DISCRIM_PIN           = BORAX_WIDETAG_PIN,
  BORAX_DISCRIM_MOVED         = BORAX_WIDETAG_MOVED,
  BORAX_DISCRIM_UNINITIALIZED = BORAX_WIDETAG_UNINITIALIZED,
};

#define BORAX_DISCRIMINATE(_obj)                       \
(BORAX_IS_POINTER(_obj)                                \
 ? BORAX_DISCRIMINATE_POINTER(BORAX_GET_POINTER(_obj)) \
 : BORAX_IS_FIXNUM(_obj)                               \
 ? BORAX_DISCRIM_FIXNUM                                \
 : BORAX_IS_CHARACTER(_obj)                            \
 ? BORAX_DISCRIM_CHARACTER                             \
 : (_obj))

#define BORAX_DISCRIMINATE_POINTER(_ptr) \
(BORAX_IS_CONS(_ptr) ? BORAX_DISCRIM_CONS : (_ptr)->WideTag)

#define BORAX_ALIGNMENT  (2 * sizeof(UINTN))
#define BORAX_ALIGN(_n)  ALIGN_VALUE(_n, BORAX_ALIGNMENT)

STATIC_ASSERT (
  BORAX_ALIGNMENT >= (1 << 3),
  "Need a minimum alignment of 8 to accomodate 3-bit tags"
  );

typedef struct {
  BORAX_OBJECT    Car;
  BORAX_OBJECT    Cdr;
} BORAX_CONS;

STATIC_ASSERT (
  sizeof (BORAX_CONS) == BORAX_ALIGNMENT,
  "Cons cell must be two words"
  );

typedef union {
  struct {
    UINT8    WideTag;
    UINT8    GcData;
  };

  struct {
    UINTN    HeaderWords[2];
    UINTN    BodyWords[];
  };
} BORAX_OBJECT_HEADER;

STATIC_ASSERT (
  sizeof (BORAX_OBJECT_HEADER) == BORAX_ALIGNMENT,
  "Object header must be two words"
  );

/*
 * Cons cell management
 * ====================
 *
 * A consequence of the object representation scheme is that while proper
 * objects may contain garbage collection metadata, cons cells cannot. However,
 * cons cells are fixed-size, which makes it feasible to aggregate their
 * metadata into the header blocks of their respective pages.
 *
 * Cons cell pages are arranged into a linked list, the root of which is owned
 * by the allocator. Because the allocator implements compacting garbage
 * collection for cons cells, it is not neccessary to maintain a free list;
 * instead, a "next free cell" pointer records the page and offset of the memory
 * location available for cons cell storage.
 *
 * Cons cell pages are normally allocated one at a time, but the allocator
 * implements support for contiguous pages to support importing external data.
 *
 * The garbage collector uses the tri-color abstraction to implement a
 * mark-and-copy cycle. When a cons cell is marked, its address is truncated to
 * to locate the page header containing the mark bitmap. As a consequence, cons
 * cells pages are allocated one-at-a-time, with no attempt made to allocate
 * contiguous pages.
 *
 * During the mark phase, live cons cells are copied to new pages to compact
 * them and to improve data locality where possible. The old locations are
 * overwritten with the CAR indicating a moved object and the CDR recording the
 * new location. This occurs once on transition from white to grey; incoming
 * pointers to the old location are updated when their containing objects
 * transition from grey to black.
 *
 * Note that during the mark phase, old cons cell pages will only contain white
 * and grey (moved-from) cells, while new pages will only contain grey
 * (moved-to) and black cells. This reduces the mark storage requirement to one
 * bit per cons cell. By setting the bit for grey and clearing to for black and
 * white, the cons cells in the new pages will transition from black to white on
 * the next collection cycle without needing to touch the bitmaps. This requires
 * an additional bit per page indicating which space it belongs to.
 *
 * The sweep phase consists of discarding the old cons cell pages.
 *
 * Future directions:
 *
 * - The moved-cell abstraction prevents us from reclaiming old pages until the
 *   collection cycle is complete; therefore in the worst case we can only be
 *   certain that we will have enough free memory to complete the copy when we
 *   have used up to around half of available memory. This could be mitigated by
 *   switching to a compacting strategy when memory is low.
 *
 * - Generational garbage collection?
 *
 * - Incremental garbage collection?
 */

#define BORAX_PAGE_SIZE          4096
#define BORAX_WORD_BITS          (CHAR_BIT * sizeof(UINTN))
#define BORAX_CONS_PER_PAGE      (BORAX_PAGE_SIZE / sizeof(BORAX_CONS))
#define BORAX_CONS_BITMAP_WORDS  (BORAX_CONS_PER_PAGE / BORAX_WORD_BITS)

typedef struct _BORAX_CONS_PAGE BORAX_CONS_PAGE;

struct _BORAX_CONS_PAGE {
  BORAX_CONS_PAGE    *Next;
  BORAX_HALFWORD     Pages;
  BORAX_HALFWORD     SpaceParity;
  UINTN              GreyBitmap[BORAX_CONS_BITMAP_WORDS];
};

#define BORAX_CONS_FIRST_INDEX  (BORAX_ALIGN(sizeof(BORAX_CONS_PAGE)))

typedef struct {
  BORAX_CONS_PAGE    *Pages;
  UINTN              FillIndex; // Offset into the head page
} BORAX_CONS_ALLOCATOR;

/*
 * Variable-width objects
 * ======================
 *
 * The implementation of cons cell memory management relies in several ways on
 * the fixed width of cons cells. In general, lisp objects can be arbitrarily
 * large, and we must handle this separately. Three key observations inform the
 * handling of variable-width data:
 *
 * - We can store garbage collection metadata within objects themselves. This is
 *   undesirable for cons cells because they are packed full of meaningful
 *   pointer bits, and the available low-order bits are already used for
 *   tags. In contrast, proper object headers have space for metadata. The
 *   upshot of this is that we do not need to be able to compute the page
 *   address from an object pointer, so we can allocate contiguous spans of
 *   pages.
 *
 * - Our mark-and-copy strategy means we do not have to design our allocator
 *   around memory fragmentation; as with cons cells, a "next free location"
 *   pointer is sufficient.
 *
 * - Lisp objects have no particular alignment requirements above the natural
 *   alignment of machine words and the pointer tagging scheme discussed above.
 *
 * Each contiguous page chunk allocated for proper object storage begins empty
 * (less the header) and is gradually filled. Eventually, the chunk will reach a
 * point at which it is no longer possible to fit new large objects, but there
 * is still space for small objects. To prevent this space from being wasted, we
 * define a finite set of bins corresponding to ranges of object size, and bin
 * each chunk according to the largest allocation it can still serve.
 *
 * When no existing chunk has enough space for an allocation, we must allocate a
 * new chunk, which raises the question of how many pages to request for the
 * chunk. We default to requesting a single page under the following
 * assumptions:
 *
 * - In the UEFI environment we're targeting, allocating in chunks confers no
 *   particular benefit in terms of memory usage or performance.
 *
 * - Most objects will be much smaller than one page.
 *
 * Some objects are simply too large to fit in a single page; for such an
 * allocation we request enough contiguous pages to store that object, then bin
 * its remainder as usual.
 *
 * The garbage collection for variable-width data is the same mark-and-copy
 * strategy we use for cons cells. The main difference is that we store the
 * metadata directly in each object rather than in the page (chunk) header. Note
 * that our choice of moved pointer representation limits object size to a
 * minimum of two machine words.
 *
 * (Not yet implemented): We make one optimization for large objects. Our
 * allocation strategy ensures that objects above a certain size (slightly less
 * than the size of a page) will only ever be located at the beginning of a page
 * chunk. Such an object may be followed by small objects at the end of the
 * chunk, and the latter will be copied normally, but we defer copying the large
 * objects until the end of the copy phase. At that point, any page chunks
 * containing a large object will contain only that large object, at exactly the
 * beginning, with no live objects following it, and we can simply reset the
 * fill pointer accordingly and move the chunk from the old set into the new set
 * by pointer assignment.
 */

typedef enum {
  BORAX_OBJECT_GCDATA_SPACEBIT = 1,
  BORAX_OBJECT_GCDATA_GREYBIT  = 2,
} BORAX_OBJECT_GCDATA;

typedef struct _BORAX_OBJECT_CHUNK BORAX_OBJECT_CHUNK;

struct _BORAX_OBJECT_CHUNK {
  BORAX_OBJECT_CHUNK    *Next;
  BORAX_HALFWORD        FillIndex;
  BORAX_HALFWORD        Pages;
};

#define BORAX_OBJECT_FIRST_INDEX  (BORAX_ALIGN(sizeof(BORAX_OBJECT_CHUNK)))

enum {
  BORAX_ALLOC_BIN_FULL  = 0,
  BORAX_ALLOC_BIN_64    = 64,
  BORAX_ALLOC_BIN_128   = 128,
  BORAX_ALLOC_BIN_256   = 256,
  BORAX_ALLOC_BIN_512   = 512,
  BORAX_ALLOC_BIN_1024  = 1024,
  BORAX_ALLOC_BIN_2048  = 2048,
  BORAX_ALLOC_BIN_MAX   = 3072,  // A chunk will never have 4 kiB free
  BORAX_ALLOC_BIN_COUNT = 8,
};

typedef struct {
  BORAX_OBJECT_CHUNK    *Chunks[BORAX_ALLOC_BIN_COUNT];
} BORAX_OBJECT_ALLOCATOR;

/*
 * Pinning objects
 * ===============
 *
 * The garbage collection scheme described above may alter the addresses of
 * objects concurrent to program execution. This does not impact lisp programs
 * since internal pointers are atomically (from the perspective of the program)
 * updated to point to the new locations. This does however pose a problem for C
 * interoperability: any pointers passed to the UEFI runtime must remain valid
 * for as long as they may be referenced.
 *
 * There are two possible solutions to this problem: fix the location of any
 * objects that will be referenced externally; or do not allow direct object
 * references to escape the virtual machine. To simplify garbage collection, we
 * opt for the latter approach (or to be precise: we do both).
 *
 * When the virtual machine needs to supply a reference to an object O to the C
 * environment, it allocates a pin object P that refers to O. P itself is
 * managed by the garbage collector separately from other objects; it will
 * remain at a stable address for the duration of its lifetime. P's internal
 * reference to O will be traced and updated by the garbage collector like any
 * other object reference.
 *
 * The external system may hold a reference to P for the duration of its
 * lifetime, but it may only access O and its descendants between garbage
 * collection cycles. In particular, any object references obtained through P
 * will be invalidated by a collection cycle. This limits, for instance, the
 * ways a TPL_CALLBACK routine can interact with the lisp system, which runs the
 * garbage collector at TPL_APPLICATION.
 */

typedef union _BORAX_PIN BORAX_PIN;

union _BORAX_PIN {
  // Pins are allocated manually and kept in a singly-linked list
  BORAX_OBJECT_HEADER    Header;
  struct {
    BORAX_HALFWORD    HalfWord0;
    BORAX_HALFWORD    Live;
    BORAX_PIN         *Next;
    BORAX_OBJECT      Object;
  };
};

/*
 * Weak pointers
 * =============
 *
 * Weak pointers are implemented as normal lisp objects whose contents are
 * treated specially by the garbage collector. Weak pointers contain two fields:
 * a "next" pointer that allows the garbage collector to visit all weak pointers
 * at the end of the copy phase; and a "value" pointer that constitutes the weak
 * reference itself.
 *
 * During the mark phase, the value pointer is not used to extend the grey
 * set. This ensures that any weakly-referenced objects remain unmarked. At the
 * end of the copy phase, the weak pointer list is scanned for defunct referents
 * and marks them unbound. Any unmarked weak pointers are removed from the list.
 */

typedef union _BORAX_WEAK_POINTER BORAX_WEAK_POINTER;

union _BORAX_WEAK_POINTER {
  BORAX_OBJECT_HEADER    Header;
  struct {
    UINTN                 Word0;
    BORAX_WEAK_POINTER    *Next;
    BORAX_OBJECT          Value;
  };
};

/*
 * Records
 * =======
 *
 * Records are a generalization of simple vectors and instances and come in two
 * flavors: "word records," which contain arbitrary data not traced by the
 * garbage collector; and "object records," which contain objects that the
 * garbage collector does trace.
 *
 * Records reference a class object that describes to higher levels of the
 * runtime how to interpret the contents of the record. The garbage collector
 * does not make use of the Class field except to trace it. The length of the
 * record in words is fixed at object creation time. Some types like bit vectors
 * need to record the length at finer than word granularity; the LengthAux field
 * is provided for this purpose.
 */

typedef union {
  BORAX_OBJECT_HEADER    Header;
  struct {
    BORAX_HALFWORD    HalfWord0;
    BORAX_HALFWORD    LengthAux; // Extra length bits for sub-word granularity
    UINTN             Length;    // Count of words
    BORAX_OBJECT      Class;
    UINTN             Data[];
  };
} BORAX_RECORD;

#define BORAX_RECORD_LENGTH(_type) \
((sizeof (_type) - sizeof (BORAX_RECORD)) / sizeof (BORAX_OBJECT))

/*
 * Triggering garbage collection
 * =============================
 *
 * The simplest garbage collection policy would be to trigger a collection cycle
 * when available memory is exhausted. There are two problems with this policy:
 *
 * 1. Since we're using a copying collector, waiting until all memory has been
 *    used would make it impossible to run a collection cycle.
 *
 * 2. We are targeting interactive applications, and waiting until memory is
 *    exhausted defers all the collection work to one point in time, which would
 *    cause large latency spikes.
 *
 * The first problem can be solved by triggering when half of memory is
 * exhausted, but this raises a third problem: when the reachable set approaches
 * but does not exceed half of memory, the collector will be triggered more and
 * more frequently, reducing the CPU time available to the application.
 *
 * A full solution to this third problem would require a non-copying fallback
 * strategy, but we can partially mitigate it and the second problem at the same
 * time with an exponential back-off strategy: we trigger a collection cycle
 * when used memory exceeds the size of the reachable set at the end of the
 * previous collection cycle, multiplied by some constant factor (greater than
 * one). This reduces the latency of a collection cycle when the reachable set is
 * small and prevents the garbage collector from repeatedly thrashing against a
 * set memory limit.
 *
 * The downside of this strategy is that it can result in an out-of-memory
 * condition when the reachable set is smaller than half of memory. Essentially,
 * we are betting that even when used memory exceeds half of memory, the
 * reachable set will be small enough to fit in free memory.
 *
 * Future directions:
 *
 * - In an interactive application, most time will be spent idling waiting for
 *   I/O to complete, and this is the perfect opportunity to run a collection
 *   cycle. Ideally, an I/O completion would pre-empt the cycle, suggesting that
 *   this is best implemented as part of an incremental garbage collector.
 */

typedef struct {
  BORAX_CONS_ALLOCATOR      Cons;
  BORAX_OBJECT_ALLOCATOR    Object;
  BORAX_WEAK_POINTER        *WeakPointers;          // Non-owning pointer
} BORAX_COPY_SPACE;

typedef struct {
  BORAX_COPY_SPACE                   FromSpace;
  BORAX_COPY_SPACE                   ToSpace;
  UINTN                              ToSpaceParity;
  BORAX_PIN                          *Pins;
  BORAX_SYSTEM_ALLOCATOR_PROTOCOL    *SysAlloc;
  UINTN                              UsedPages;
} BORAX_ALLOCATOR;

VOID
EFIAPI
BoraxAllocatorInit (
  OUT BORAX_ALLOCATOR                 *Alloc,
  IN BORAX_SYSTEM_ALLOCATOR_PROTOCOL  *SysAlloc
  );

VOID
EFIAPI
BoraxAllocatorCleanup (
  IN BORAX_ALLOCATOR  *Alloc
  );

EFI_STATUS
EFIAPI
BoraxAllocatorCollect (
  IN BORAX_ALLOCATOR  *Alloc
  );

VOID *
EFIAPI
BoraxAllocateExternalPages (
  IN BORAX_ALLOCATOR  *Alloc,
  IN UINTN            Pages
  );

VOID
EFIAPI
BoraxFreeExternalPages (
  IN BORAX_ALLOCATOR  *Alloc,
  IN VOID             *Buffer,
  IN UINTN            Pages
  );

VOID
EFIAPI
BoraxInjectExternalConsPages (
  IN BORAX_ALLOCATOR  *Alloc,
  IN VOID             *Buffer,
  IN UINTN            Pages
  );

EFI_STATUS
EFIAPI
BoraxInjectExternalObjectPages (
  IN BORAX_ALLOCATOR  *Alloc,
  IN VOID             *Buffer,
  IN UINTN            Pages
  );

EFI_STATUS
EFIAPI
BoraxAllocateCons (
  IN BORAX_ALLOCATOR  *Alloc,
  IN BORAX_OBJECT     Car,
  IN BORAX_OBJECT     Cdr,
  OUT BORAX_CONS      **Cons
  );

EFI_STATUS
EFIAPI
BoraxAllocateObject (
  IN BORAX_ALLOCATOR       *Alloc,
  IN UINTN                 Size,
  OUT BORAX_OBJECT_HEADER  **Object
  );

EFI_STATUS
EFIAPI
BoraxAllocatePin (
  IN BORAX_ALLOCATOR  *Alloc,
  IN BORAX_OBJECT     Object,
  OUT BORAX_PIN       **Pin
  );

VOID
EFIAPI
BoraxReleasePin (
  IN BORAX_PIN  *Pin
  );

EFI_STATUS
EFIAPI
BoraxAllocateWeakPointer (
  IN BORAX_ALLOCATOR      *Alloc,
  IN BORAX_OBJECT         Object,
  OUT BORAX_WEAK_POINTER  **WeakPointer
  );

EFI_STATUS
EFIAPI
BoraxAllocateRecord (
  IN BORAX_ALLOCATOR  *Alloc,
  IN UINTN            WideTag,  // WORD_RECORD or OBJECT_RECORD
  IN BORAX_OBJECT     Class,
  IN UINTN            Length,
  IN BORAX_HALFWORD   LengthAux,
  IN UINTN            InitialElement,
  OUT BORAX_RECORD    **Record
  );

EFI_STATUS
EFIAPI
BoraxAllocateRecordUninitialized (
  IN BORAX_ALLOCATOR  *Alloc,
  IN UINTN            WideTag,  // WORD_RECORD or OBJECT_RECORD
  IN BORAX_OBJECT     Class,
  IN UINTN            Length,
  IN BORAX_HALFWORD   LengthAux,
  OUT BORAX_RECORD    **Record
  );

#endif // BORAX_MEMORY_H
