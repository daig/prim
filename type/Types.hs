{-# language NoImplicitPrelude #-}
{-# language StandaloneKindSignatures #-}
module Types (module Types, module X) where
import GHC.Prim as X
import GHC.Types as X (TYPE,Levity(..),RuntimeRep(..),VecCount(..),VecElem(..),Constraint)

-- | The kind constructor of types abstracted over 'RuntimeRep'
type T = TYPE
type T_ = TYPE (BoxedRep Unlifted)
type T0 = TYPE (TupleRep '[])
type T# l = TYPE (BoxedRep l)

-- | The kind of a type
type K (a ∷ k) = k
-- | The 'RuntimeRep' of a type
type R# (i ∷ T r) = r

type C = Constraint

newtype B ∷ K I where B# ∷ {unB ∷ I} → B
pattern F, T ∷ B
pattern F = B# 0#
pattern T = B# 1#
{-# complete F, T #-}

-- | A number less-than, equal-to, or greater-than @0#@
newtype Ordering ∷ K I where Ordering# ∷ I → Ordering
pattern LT ∷ Ordering
pattern LT ← ((\ (Ordering# i) → i <#  0# ) → 1# ) where LT = Ordering# -1#
pattern EQ ← ((\ (Ordering# i) → i ==# 0# ) → 1# ) where EQ = Ordering#  0#
pattern GT ← ((\ (Ordering# i) → i >#  0# ) → 1# ) where GT = Ordering#  1#
{-# complete LT, EQ, GT #-}


-- | 31-bit Unicode code points
type Char = Char#

-- | 8-bit Latin-1 code points
newtype Char8 ∷ K U where Char8# ∷ Char → Char8

type I = Int#
type I8 = Int8#
-- | Narrow a machine 'I' to 8 bits
pattern I8 ∷ I → I8
pattern I8 u ← (int8ToInt# → u) where I8 = intToInt8#
{-# complete I8 #-}

type I16 = Int16#
-- | Narrow a machine 'I' to 8 bits
pattern I16 ∷ I → I16
pattern I16 u ← (int16ToInt# → u) where I16 = intToInt16#
{-# complete I16 #-}

type I32 = Int32#
-- | Narrow a machine 'I' to 32 bits
pattern I32 ∷ I → I32
pattern I32 u ← (int32ToInt# → u) where I32 = intToInt32#
{-# complete I32 #-}

type I64 = Int64#
-- | Narrow a machine 'I' to 64 bits
pattern I64 ∷ I → I64
pattern I64 u ← (int64ToInt# → u) where I64 = intToInt64#
{-# complete I64 #-}

type U = Word#

type U8 = Word8#
-- | Narrow a machine 'U' to 8 bits
pattern U8 ∷ U → U8
pattern U8 u ← (word8ToWord# → u) where U8 = wordToWord8#
{-# complete U8 #-}

type U16 = Word16#

-- | Narrow a machine 'U' to 16 bits
pattern U16 ∷ U → U16
pattern U16 u ← (word16ToWord# → u) where U16 = wordToWord16#
{-# complete U16 #-}

type U32 = Word32#
-- | Narrow a machine 'U' to 32 bits
pattern U32 ∷ U → U32
pattern U32 u ← (word32ToWord# → u) where U32 = wordToWord32#
{-# complete U32 #-}

type U64 = Word64#
-- | Narrow a machine 'U' to 64 bits
pattern U64 ∷ U → U64
pattern U64 u ← (word64ToWord# → u) where U64 = wordToWord64#
{-# complete U64 #-}

type F32 = Float#
type F64 = Double#

type ST s (a ∷ T ra) = State# s → (# State# s , a #)
type ST_ s = State# s → State# s

-- | @(☸)@ is the primitive, unlifted type of realworld state.
-- It's only purpose is to sequence IO actions.
-- It is represented by nothing at all. 
type (☸) = State# RealWorld


-- | A computation performing some I\/O before returning a value of type @a@.
type IO (a ∷ T r)  = ST RealWorld a
-- | A computation performing some I\/O
type IO_ = ST_ RealWorld


type Small :: forall {l :: Levity} {k}.
              (T# l -> k) -> T# l -> k
type family Small a = sa | sa -> a where
  Small A = A_Small
  Small M_A = M_A_Small

type A_Small :: forall {l :: Levity}. T# l -> T_
newtype A_Small x = SmallArray# (SmallArray# x)

type M_A_Small :: forall {l :: Levity}. T# l -> * -> T_
newtype M_A_Small x s = M_SmallArray# (SmallMutableArray# s x)

type A :: forall {l :: Levity}. T# l -> T_
newtype A x = Array# (Array# x)


type M_A :: forall {l :: Levity}. T# l -> * -> T_
newtype M_A x s = M_Array# (MutableArray# s x)

-- Unpinned
newtype A# = UnpinnedByteArray# ByteArray#

-- | An unboxed vector with offset and length
newtype V# = V# (# A# , I , I #)

type M_A# :: * -> T_
newtype M_A# s = M_UnpinnedByteArray# (MutableByteArray# s)

type A_Pinned# :: T_
newtype A_Pinned# = PinnedByteArray# ByteArray#

type M_A_Pinned# :: * -> T_
newtype M_A_Pinned# s = M_PinnedByteArray# (MutableByteArray# s)

type A_ :: forall (r :: RuntimeRep). T r -> T_
newtype A_ x = A# A#

type M_A_ :: forall (r :: RuntimeRep). T r -> * -> T_
newtype M_A_ x s = M_A# (M_A# s)

type A_Pinned_ :: forall (r :: RuntimeRep). T r -> T_
newtype A_Pinned_ x = A_Pinned# A_Pinned#

type M_A_Pinned_ :: forall (r :: RuntimeRep). T r -> * -> T_
newtype M_A_Pinned_ x s = M_A_Pinned# (M_A_Pinned# s)

type M :: forall {r :: RuntimeRep} {t :: *}. TYPE r -> t
type family M a = ma | ma → a where
  M A# = M_A#
  M (A_ x) = M_A_ x
  M A_Pinned# = M_A_Pinned#
  M (A_Pinned_ x) = (M_A_Pinned_ x)
  M (A_Small x) = M_A_Small x
  M (A x) = M_A x
  M (☸) = (☸)
  M P# = P#
  M (P_ (x ∷ T r)) = P_ x
  M (P s x) = P s x
  M (P_Async s x) = P_Async s x
  M (P_Sync s x) = P_Sync s x
  M (P_Weak x) = P_Weak x
  M (P_Stable x) = P_Stable x

-- | A C-style null-terminated string
newtype S# ∷ K P# where S# ∷ Addr# → S#

type P# = Addr#
type P_ :: forall {r :: RuntimeRep}. T r -> T AddrRep
newtype P_ x = P# P#

type P = MutVar#
type P_Async = TVar#
-- | A synchronising variable, used for communication between concurrent threads.
-- It can be thought of as a box, which may be empty or full.
--
-- The RTS implementation is really an abstraction for
-- connecting 'take' and 'write' calls between threads
type P_Sync = MVar#

{-|
A weak pointer expressing a relashionship between a /key/ and a /value/ of type @v@:
if the value is alive to the GC if the key is.
A reference from the value to the key does /not/ keep the key alive.
A weak pointer may also have a finalizer of type @IO_@; if it does,
then the finalizer will be run at most once, at a time after the key
has become unreachable by the program (\"dead\").  The storage manager
attempts to run the finalizer(s) for an object soon after the object
dies, but promptness is not guaranteed.
It is not guaranteed that a finalizer will eventually run, and no
attempt is made to run outstanding finalizers when the program exits.
Therefore finalizers should not be relied on to clean up resources -
other methods (eg. exception handlers) should be employed, possibly in
addition to finalizers.
References from the finalizer to the key are treated in the same way
as references from the value to the key: they do not keep the key
alive.  A finalizer may therefore ressurrect the key, perhaps by
storing it in the same data structure.
The finalizer, and the relationship between the key and the value,
exist regardless of whether the program keeps a reference to the
'Weak' object or not.
Finalizers for multiple @Weak.P@ on the same key are run in arbitrary order, or perhaps concurrently.
You must ensure there's only one finalizer if the finalizer relies on that fact.
If there are no other threads to run, the RTS will check
for runnable finalizers before declaring the system to be deadlocked.
WARNING: weak pointers to ordinary non-primitive Haskell types are
particularly fragile, because the compiler is free to optimise away or
duplicate the underlying data structure.  Therefore attempting to
place a finalizer on an ordinary Haskell type may well result in the
finalizer running earlier than you expected.  This is not a problem
for caches and memo tables where early finalization is benign.
Finalizers /can/ be used reliably for types that are created explicitly
and have identity, such as @ST.P@ and @Sync.P@.
-}
type P_Weak = Weak#
type P_Stable = StablePtr#

type Name = StableName#

-- | Primitive maybe type represented by a tag and (possibly invalid) value.
type Maybe# (a ∷ T r)  = (# B , a #)
