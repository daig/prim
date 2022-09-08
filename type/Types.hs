{-# language NoImplicitPrelude #-}
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

type Rep = RuntimeRep

type C = Constraint

newtype B where B# ∷ {unB ∷ I} → B
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

-- | Single-precision floating point numbers.
-- It is desirable that this type be at least equal in range and precision
-- to the IEEE single-precision type.
type F32 = Float#
-- | Double-precision floating point numbers.
-- It is desirable that this type be at least equal in range and precision
-- to the IEEE double-precision type.
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

newtype Transaction = Transaction RealWorld
type STM# (a ∷ T r)  = ST Transaction a
type STM_# = ST_ Transaction

type Small :: forall {l :: Levity} {k}.
              (T# l -> k) -> T# l -> k
type family Small a = sa | sa -> a where
  Small A_Box = A_Box_Small
  Small A_Box_M = A_Box_Small_M

type A_Box_Small :: forall {l :: Levity}. T# l -> T_
newtype A_Box_Small x = SmallArray# (SmallArray# x)

type A_Box_Small_M :: forall {l :: Levity}. T# l -> * -> T_
newtype A_Box_Small_M x s = SmallArray_M# (SmallMutableArray# s x)

type A_Box :: forall {l :: Levity}. T# l -> T_
newtype A_Box x = Array# (Array# x)


type A_Box_M :: forall {l :: Levity}. T# l -> * -> T_
newtype A_Box_M x s = MutableArray# (MutableArray# s x)

-- Unpinned
newtype Bytes = UnpinnedByteArray# ByteArray#

-- | An unboxed vector with offset and length
newtype Buffer = Bytes_Off_Len# (# ByteArray# , I , I #)
newtype Buffer_Pinned = PinnedBytes_Off_Len# (# ByteArray# , I , I #)

type Bytes_M :: * -> T_
newtype Bytes_M s = M_UnpinnedByteArray# (MutableByteArray# s)

type Bytes_Pinned :: T_
newtype Bytes_Pinned = PinnedByteArray# ByteArray#

type Bytes_Pinned_M :: * -> T_
newtype Bytes_Pinned_M s = M_PinnedByteArray# (MutableByteArray# s)

type A_Unbox :: forall {r :: Rep}. T r -> T_
newtype A_Unbox x = Bytes Bytes

type A_Unbox_M :: forall (r :: Rep). T r -> * -> T_
newtype A_Unbox_M x s = Bytes_M (Bytes_M s)

type A_Unbox_Pinned :: forall (r :: Rep). T r -> T_
newtype A_Unbox_Pinned x = Bytes_Pinned Bytes_Pinned

type A_Unbox_Pinned_M :: forall (r :: Rep). T r -> * -> T_
newtype A_Unbox_Pinned_M x s = Bytes_Pinned_M (Bytes_Pinned_M s)

type A :: forall {r :: Rep}. T r -> T_
type family A x = a where
  A (x :: *) = A_Box x
  A (x :: T# Unlifted) = A_Box x
  A (x :: T IntRep) = A_Unbox x
  A (x :: T Int8Rep) = A_Unbox x
  A (x :: T Int16Rep) = A_Unbox x
  A (x :: T Int32Rep) = A_Unbox x
  A (x :: T Int64Rep) = A_Unbox x
  A (x :: T WordRep) = A_Unbox x
  A (x :: T Word8Rep) = A_Unbox x
  A (x :: T Word16Rep) = A_Unbox x
  A (x :: T Word32Rep) = A_Unbox x
  A (x :: T Word64Rep) = A_Unbox x
  A (x :: T FloatRep) = A_Unbox x
  A (x :: T DoubleRep) = A_Unbox x
  A (x :: T AddrRep) = A_Unbox x

type M :: forall {r :: Rep}. T r -> * -> T r
type family M a = ma | ma → a where
  M Bytes = Bytes_M
  M (A_Unbox x) = A_Unbox_M x
  M Bytes_Pinned = Bytes_Pinned_M
  M (A_Unbox_Pinned x) = (A_Unbox_Pinned_M x)
  M (A_Box_Small x) = A_Box_Small_M x
  M (A_Box x) = A_Box_M x
  M ByteArray# = MutableByteArray#
  M P# = M#

{-
  M (☸) = (☸)
  M P# = P#
  M (P_ (x ∷ T r)) = P_ x
  M (P s x) = P s x
  M (P_Async s x) = P_Async s x
  M (P_Sync s x) = P_Sync s x
  M (P_Weak x) = P_Weak x
  M (P_Stable x) = P_Stable x
-}

-- | A C-style null-terminated string
type S# = Addr#

-- | An arbitrary machine address assumed to point outside the garbage-collected heap
type P# = Addr#
newtype M# s = Mutable P#
type P_Unbox :: forall {r :: Rep}. T r -> T AddrRep
newtype P_Unbox x = P# P#

type P_Box = MutVar#
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

type P_Stable_Name = StableName#

-- | Primitive maybe type represented by a tag and (possibly invalid) value.
type Maybe# (a ∷ T r)  = (# B , a #)
