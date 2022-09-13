--------------------------------------------------------------------
-- | Description : Compact Serialized Regions outside the GC
--
-- see <http://ezyang.com/papers/ezyang15-cnf.pdf Efficient Communication and Collection with Compact Normal Forms>
--------------------------------------------------------------------
module RTS.Compact where


-- | Contains fully evaluated immutable data.
--
-- Data in a @Compact@ is not tracked by the GC, the whole region is alive or dead.
--
-- Data in a @Compact@ can be deserialized at any time by the same binary that
-- produced it.
type Compact = Compact#
type Block# = (# P#, U #) -- ^ Address and Utilized Size (bytes)

-- | Create a new CNF with a single compact block.
-- The argument is the capacity of the compact block (in bytes, not words).
-- The capacity is rounded up to a multiple of the allocator block size and is capped to one mega block.
new ∷ U → IO Compact
new = compactNew#
-- | Set the new allocation size of the CNF. This value (in bytes) determines
-- the capacity of each compact block in the CNF. It does not retroactively
-- affect existing compact blocks in the CNF.
resize ∷ Compact → U → IO_
resize = compactResize#
-- | Containment
(∈) ∷ a → Compact → IO B
a ∈ c = coerce do compactContains# c a

-- | Is it contained in any live 'Compact'?
inAny' ∷ a → IO B
inAny' a = coerce do compactContainsAny# a

-- | Get the first block
head ∷ Compact → IO Block#
head c s0 = case compactGetFirstBlock# c s0 of
  (# s1, a, n #) → (# s1, (# a, n #) #)

-- | Get the next block. 'Null' if it's the last.
--
-- Warning: Address must really be in the compact.
next# ∷ Compact → P# {- ^ Block address -} → IO Block#
next# c a0 s0 = case coerce compactGetNextBlock# c a0 s0 of
  (# s1, a1, n #) → (# s1, (# a1, n #) #)

-- | Attempt to allocate a compact block with the capacity (in bytes) given by
-- the first argument. The Addr# is a pointer to previous compact block of the
-- CNF or nullAddr# to create a new CNF with a single compact block.
--
-- The resulting block is not known to the GC until 'fixup' is
-- called on it, and care must be taken so that the address does not escape or
-- memory will be leaked.
allocate ∷ Block# → IO P#
allocate (# a, n #) = coerce compactAllocateBlock# n a

fixup ∷ P# → P# → IO (# Compact, P# #)
fixup b0 root s0 = case compactFixupPointers# b0 root s0 of
  (# s1, c, b1 #) → (# s1, (# c, b1 #) #)

insert, insertShared ∷ a → Compact → IO a
insert a c = compactAdd# c a
insertShared a c = compactAddWithSharing# c a

size ∷ Compact → IO U
size = coerce compactSize#
