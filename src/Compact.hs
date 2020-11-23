module Compact where
import qualified P.Byte as Byte

type Compact = Compact#
type Block# = (# Byte.P, U64 #)

new ∷ U64 → IO# Compact
new = coerce compactNew#
resize ∷ Compact → U64 → IO_#
resize = coerce compactResize#
elem' ∷ a → Compact → IO# B#
elem' a c = compactContains# c a

elemOfAny' ∷ a → IO# B#
elemOfAny' = compactContainsAny#

-- | The address and size (in bytes) of the first block of a @Compact@
head ∷ Compact → IO# Block#
head c s0 = case compactGetFirstBlock# c s0 of
  (# s1, a, n #) → (# s1, (# a, U64 n #) #)

next ∷ Compact → Byte.P → IO# Block#
next c a0 s0 = case coerce compactGetNextBlock# c a0 s0 of
  (# s1, a1, n #) → (# s1, (# a1, n #) #)

allocate ∷ Block# → IO# Byte.P
allocate (# a, n #) = coerce compactAllocateBlock# n a

fixup ∷ Byte.P → Byte.P → IO# (# Compact, Byte.P #)
fixup b0 root s0 = case compactFixupPointers# b0 root s0 of
  (# s1, c, b1 #) → (# s1, (# c, b1 #) #)

insert, insertShared ∷ a → Compact → IO# a
insert a c = compactAdd# c a
insertShared a c = compactAddWithSharing# c a

size ∷ Compact → IO# U64
size = coerce compactSize#
