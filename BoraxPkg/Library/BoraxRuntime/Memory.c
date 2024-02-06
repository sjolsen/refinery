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
 *   0x03  ...
 *   0x07  ...
 *   ...
 *   0xFB  moved
 *   0xFF  uninitialized
 *
 * Here, 0xFF has been chosen as the uninitialized marker to allow initializing
 * raw memory with memset (the C idiom is to initialize with 0x00, but we've
 * already assigned that encoding to fixnums).
 *
 * TODO: Remainder of heap object representation.
 *
 * Cons cell management
 * ====================
 *
 * A consequence of the object representation scheme is that while proper
 * objects may contain runtime metadata, cons cells cannot. However, cons cells
 * are fixed-size, which makes it feasible to aggregate their metadata into the
 * header blocks of their respective pages.
 *
 * Cons cell pages are arranged into a linked list, the root of which is owned
 * by the allocator. Because the allocator implements compacting garbage
 * collection for cons cells, it is not neccessary to maintain a free list;
 * instead, a "next free cell" pointer records the page and offset of the memory
 * location available for cons cell storage.
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
 * the next collection cycle without needing to touch the bitmaps.
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
 *
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
 * We make one optimization for large objects. Our allocation strategy ensures
 * that objects above a certain size (slightly less than the size of a page)
 * will only ever be located at the beginning of a page chunk. Such an object
 * may be followed by small objects at the end of the chunk, and the latter will
 * be copied normally, but we defer copying the large objects until the end of
 * the copy phase. At that point, any page chunks containing a large object will
 * contain only that large object, at exactly the beginning, with no live
 * objects following it, and we can simply reset the fill pointer accordingly
 * and move the chunk from the old set into the new set by pointer assignment.
 *
 * Weak pointers and pins
 * ======================
 *
 * TODO.
 *
 * Triggering garbage collection
 * =============================
 *
 * TODO.
 */
