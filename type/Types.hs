{-# language CPP,NoImplicitPrelude #-}
module Types (module Types, module X) where
import GHC.Prim as X
import GHC.Types as X (TYPE,Levity(..),RuntimeRep(..),VecCount(..),VecElem(..),Constraint,Any)
import GHC.Prim.Panic as X
import Unsafe.Coerce (unsafeCoerce#)
import GHC.Types qualified as GHC
import GHC.Word  qualified as GHC
import GHC.Int  qualified as GHC
import GHC.Num.BigNat
import GHC.Num.Natural

type N = Natural
type Nat = BigNat#

-- | The kind constructor of types abstracted over 'RuntimeRep'
type T = TYPE
type T_ = TYPE (BoxedRep Unlifted)
type T0 = TYPE (TupleRep '[])
type T# l = TYPE (BoxedRep l)

-- | The kind of a type
type K (a ∷ k) = k
-- | The 'RuntimeRep' of a type
type R (i ∷ T r) = r

type Rep = RuntimeRep

type C = Constraint

newtype B# = B# I
pattern F#, T# ∷ B#
pattern F# = B# 0#
pattern T# = B# 1#
{-# complete F#, T# #-}

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
newtype Char8 = Char8# Char

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

-- * Transactional Memory Operations
data Transaction

-- | A comp
type STM (a ∷ T r) = ST Transaction a
-- | Transactional Memory operations
type STM_ = ST_ Transaction

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

-- | (Possibly heterogeneous) contiguous bytes.
newtype Bytes = UnpinnedByteArray# ByteArray#

-- | An unboxed vector with offset and length.
newtype Buffer = Bytes_Off_Len# (# ByteArray# , I , I #)
newtype Buffer_Pinned = PinnedBytes_Off_Len# (# ByteArray# , I , I #)

-- | Mutable (possibly heterogeneous) contiguous bytes
type Bytes_M :: * -> T_
newtype Bytes_M s = M_UnpinnedByteArray# (MutableByteArray# s)

-- | (Possibly heterogeneous) contiguous bytes.
-- Pinned to an address and gaurenteed not to be moved by GC.
type Bytes_Pinned :: T_
newtype Bytes_Pinned = PinnedByteArray# ByteArray#

-- | Mutable (possibly heterogeneous) contiguous bytes.
-- Pinned to an address and gaurenteed not to be moved by GC.
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
-- | Primitive array type.
-- The concrete representation can be determined by the kind of its contents
type family A x = a where
  A (x :: T# _) = A_Box x
  A (x :: K I) = A_Unbox x
  A (x :: K I8) = A_Unbox x
  A (x :: K I16) = A_Unbox x
  A (x :: K I32) = A_Unbox x
  A (x :: K I64) = A_Unbox x
  A (x :: K U) = A_Unbox x
  A (x :: K U8) = A_Unbox x
  A (x :: K U16) = A_Unbox x
  A (x :: K U32) = A_Unbox x
  A (x :: K U64) = A_Unbox x
  A (x :: K F32) = A_Unbox x
  A (x :: K F64) = A_Unbox x
  A (x :: K Addr#) = A_Unbox x

type M :: forall {r ∷ Rep}. T r -> * -> T r
type family M a = ma | ma → a where
  M Bytes = Bytes_M
  M (A_Unbox x) = A_Unbox_M x
  M Bytes_Pinned = Bytes_Pinned_M
  M (A_Unbox_Pinned x) = (A_Unbox_Pinned_M x)
  M (A_Box_Small x) = A_Box_Small_M x
  M (A_Box x) = A_Box_M x
  M ByteArray# = MutableByteArray#
  M Addr# = MutableAddr#
  M (P_Unbox x) = P_Unbox_M x

-- | A C-style null-terminated string
type S# = Addr#

-- | 
type P# = Addr#

-- | An arbitrary machine address assumed to point outside the garbage-collected heap
newtype MutableAddr# s = Addr_M# Addr#

-- | An machine address to valid data, assumed to point outside the garbage-collected heap
type P_Unbox :: forall {r :: Rep}. T r -> T AddrRep
newtype P_Unbox x = Addr# Addr#
type P_Unbox_M :: forall {r :: Rep}. T r -> ★ → T AddrRep
newtype P_Unbox_M x s = MutableAddr# (MutableAddr# s)

type P_Box = MutVar#
newtype P_Async x = TVar# (TVar# Transaction x)
-- | A synchronising variable, used for communication between concurrent threads.
-- It can be thought of as a box, which may be empty or full.
--
-- The RTS implementation is really an abstraction for
-- connecting 'take' and 'write' calls between threads
type P_Sync = MVar# RealWorld

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
and have identity, such as @P_Boxed@ and @P_Sync@.
-}
type P_Weak = Weak#
type P_Stable = StablePtr#


type P_Stable_Name ∷ T# l → T_
type P_Stable_Name = StableName#

-- | Primitive maybe type represented by a tag and (possibly invalid) value.
type (?) (a ∷ T r) = (# (##) | a #)
-- | Primitive option type represented by a tag and two values of the same representation.
type Result# (a ∷ T r) = (# a | a #)

pattern Ok, Err ∷ a → Result# a
pattern Ok a = (# | a #); {-# inline Ok #-}
pattern Err b = (# b | #); {-# inline Err #-}
{-# complete Ok,Err #-}

-- | The uninhabited ("Void") type
newtype X# ∷ T (SumRep '[]) where X# ∷ X# → X#

type Box ∷ ∀ {r}. T r → ★
type family Box x = b | b → x where
  Box I = GHC.Int
  Box I8 = GHC.Int8
  Box I16 = GHC.Int16
  Box I32 = GHC.Int32
  Box I64 = GHC.Int64
  Box U = GHC.Word
  Box U8 = GHC.Word8
  Box U16 = GHC.Word16
  Box U32 = GHC.Word32
  Box U64 = GHC.Word64
  Box Ordering = GHC.Ordering
  Box B# = GHC.Bool
  Box Nat = BigNat


type VRep ∷ ∀ {r}. T r → Natural → Rep
type family VRep v n = r | r → v n where
  VRep I8  16 = R Int8X16#
  VRep I16 8  = R Int16X8#
  VRep I32 4  = R Int32X4#
  VRep I64 2  = R Int64X2#

  VRep I8  32 = R Int8X32#
  VRep I16 16 = R Int16X16#
  VRep I32 8  = R Int32X8#
  VRep I64 4  = R Int64X4#

  VRep I8 64  = R Int8X64#
  VRep I16 32 = R Int16X32#
  VRep I32 16 = R Int32X16#
  VRep I64 8  = R Int64X8#


  VRep U8  16 = R Word8X16#
  VRep U16 8  = R Word16X8#
  VRep U32 4  = R Word32X4#
  VRep U64 2  = R Word64X2#

  VRep U8  32 = R Word8X32#
  VRep U16 16 = R Word16X16#
  VRep U32 8  = R Word32X8#
  VRep U64 4  = R Word64X4#

  VRep U8  64 = R Word8X64#
  VRep U16 32 = R Word16X32#
  VRep U32 16 = R Word32X16#
  VRep U64 8  = R Word64X8#


  VRep F32 4  = R FloatX4#
  VRep F64 2  = R DoubleX2#

  VRep F32 8  = R FloatX8#
  VRep F64 4  = R DoubleX4#

  VRep F32 16 = R FloatX16#
  VRep F64 8  = R DoubleX8#

type VElem ∷ ∀ {r}. T r → VecElem
type family VElem a = e | e → a where
  VElem I8 = Int8ElemRep
  VElem I16 = Int16ElemRep
  VElem I32 = Int32ElemRep
  VElem I64 = Int64ElemRep
  VElem U8 = Word8ElemRep
  VElem U16 = Word16ElemRep
  VElem U32 = Word32ElemRep
  VElem U64 = Word64ElemRep
  VElem F32 = FloatElemRep
  VElem F64 = DoubleElemRep
type VCount ∷ Natural → VecCount
type family VCount n = c | c → n where
  VCount 2 = Vec2
  VCount 4 = Vec4
  VCount 8 = Vec8
  VCount 16 = Vec16
  VCount 32 = Vec32
  VCount 64 = Vec64

--type (×) ∷ ∀ {r} (a ∷ T r) (n ∷ Natural). T r → Natural → T (VRep a n)
type (×) ∷ ∀ (a ∷ T r) → ∀ (n ∷ Natural) → T (VecRep (VCount n) (VElem a))
type family a × n = t | t → a n where
  I8  × 16 = Int8X16#
  I16 ×  8 = Int16X8#
  I32 ×  4 = Int32X4#
  I64 ×  2 = Int64X2#

  I8  × 32 = Int8X32#
  I16 × 16 = Int16X16#
  I32 ×  8 = Int32X8#
  I64 ×  4 = Int64X4#

  I8  × 64 = Int8X64#
  I16 × 32 = Int16X32#
  I32 × 16 = Int32X16#
  I64 ×  8 = Int64X8#


  U8  × 16 = Word8X16#
  U16 ×  8 = Word16X8#
  U32 ×  4 = Word32X4#
  U64 ×  2 = Word64X2#

  U8  × 32 = Word8X32#
  U16 × 16 = Word16X16#
  U32 ×  8 = Word32X8#
  U64 ×  4 = Word64X4#

  U8  × 64 = Word8X64#
  U16 × 32 = Word16X32#
  U32 × 16 = Word32X16#
  U64 ×  8 = Word64X8#


  F32 ×  4 = FloatX4#
  F64 ×  2 = DoubleX2#

  F32 ×  8 = FloatX8#
  F64 ×  4 = DoubleX4#

  F32 × 16 = FloatX16#
  F64 ×  8 = DoubleX8#
