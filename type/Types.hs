{-# language CPP,NoImplicitPrelude #-}
module Types (Bool(F,T), B#(F#,T#), module Types, module X) where
import GHC.Prim as X
import GHC.Types as X (TYPE,Levity(..),RuntimeRep(..),VecCount(..),VecElem(..),Constraint,Any,Char(..),Int,Word,Float,Double)
import GHC.Types (Bool(..))
import GHC.Prim.Panic as X
import Unsafe.Coerce (unsafeCoerce#)
import GHC.Types qualified as GHC
import GHC.Word  qualified as GHC
import GHC.Int  qualified as GHC
import GHC.Num.BigNat
import GHC.Num.Natural

type N = Natural
newtype Nat = BigNat# (A X U)

-- | The kind constructor of types abstracted over 'RuntimeRep'
type T = TYPE
type T_ = TYPE (BoxedRep Unlifted)
type T0 = TYPE (TupleRep '[])
type T# l = TYPE (BoxedRep l)
type TC = Constraint

type B = Bool

-- | The kind of a type
type K (a ∷ k) = k
-- | The 'RuntimeRep' of a type
type R (i ∷ T r) = r

newtype B# = B# I
pattern F#, T# ∷ B#
pattern F# = B# 0#
pattern T# = B# 1#
{-# complete F#, T# #-}

pattern T,F ∷ Bool
pattern T = True
pattern F = False

-- | A number less-than, equal-to, or greater-than @0#@
newtype Ordering ∷ K I where Ordering# ∷ I → Ordering
pattern LT ∷ Ordering
pattern LT ← ((\ (Ordering# i) → i <#  0# ) → 1# ) where LT = Ordering# -1#
pattern EQ ← ((\ (Ordering# i) → i ==# 0# ) → 1# ) where EQ = Ordering#  0#
pattern GT ← ((\ (Ordering# i) → i >#  0# ) → 1# ) where GT = Ordering#  1#
{-# complete LT, EQ, GT #-}


type C# = Char#
-- | 8-bit Latin-1 code points
newtype C1# = C1# C#

type I = Int#
type I1 = Int8#
-- | Narrow a machine 'I' to 8 bits
pattern I1 ∷ I → I1
pattern I1 u ← (int8ToInt# → u) where I1 = intToInt8#
{-# complete I1 #-}

type I2 = Int16#
-- | Narrow a machine 'I' to 8 bits
pattern I2 ∷ I → I2
pattern I2 u ← (int16ToInt# → u) where I2 = intToInt16#
{-# complete I2 #-}

type I4 = Int32#
-- | Narrow a machine 'I' to 32 bits
pattern I4 ∷ I → I4
pattern I4 u ← (int32ToInt# → u) where I4 = intToInt32#
{-# complete I4 #-}

type I8 = Int64#
-- | Narrow a machine 'I' to 64 bits
pattern I8 ∷ I → I8
pattern I8 u ← (int64ToInt# → u) where I8 = intToInt64#
{-# complete I8 #-}

type U = Word#

type U1 = Word8#
-- | Narrow a machine 'U' to 8 bits
pattern U1 ∷ U → U1
pattern U1 u ← (word8ToWord# → u) where U1 = wordToWord8#
{-# complete U1 #-}

type U2 = Word16#

-- | Narrow a machine 'U' to 16 bits
pattern U2 ∷ U → U2
pattern U2 u ← (word16ToWord# → u) where U2 = wordToWord16#
{-# complete U2 #-}

type U4 = Word32#
-- | Narrow a machine 'U' to 32 bits
pattern U4 ∷ U → U4
pattern U4 u ← (word32ToWord# → u) where U4 = wordToWord32#
{-# complete U4 #-}

type U8 = Word64#
-- | Narrow a machine 'U' to 64 bits
pattern U8 ∷ U → U8
pattern U8 u ← (word64ToWord# → u) where U8 = wordToWord64#
{-# complete U8 #-}

-- | Single-precision floating point numbers.
-- It is desirable that this type be at least equal in range and precision
-- to the IEEE single-precision type.
type F4 = Float#
-- | Double-precision floating point numbers.
-- It is desirable that this type be at least equal in range and precision
-- to the IEEE double-precision type.
type F8 = Double#

type ST s (a ∷ T ra) = State# s → (# State# s , a #)
type ST' s (a ∷ T ra) = State# s → (# State# s , B#, a #)
type ST_ s = State# s → State# s

-- | A computation performing some I\/O before returning a value of type @a@.
type IO (a ∷ T r)  = ST RealWorld a
type IO' (a ∷ T ra) = State# RealWorld → (# State# RealWorld , B#, a #)
-- | A computation performing some I\/O
type IO_ = ST_ RealWorld

-- * Transactional Memory Operations
data Transaction

-- | A comp
type STM (a ∷ T r) = ST Transaction a
-- | Transactional Memory operations
type STM_ = ST_ Transaction

{-
type Small ∷ ∀ {l ∷ Levity} {k}.
              (T# l → k) → T# l → k
type family Small a = sa | sa → a where
  Small Array# = SmallArray#
  Small MutableArray# = SmallMutableArray#
  -}

type Slice ∷ ∀ {l}. T# l → K (# ByteArray#, I, I #)
newtype Slice x = Array_Off_Len# (# Array# x, I, I #)
type MutableSlice ∷ ∀ {l}. ★ → T# l → K (# ByteArray#, I, I #)
newtype MutableSlice s x = MutableArray_Off_Len# (# MutableArray# s x, I, I #)
type SmallSlice ∷ ∀ {l}. T# l → K (# ByteArray#, I, I #)
newtype SmallSlice x = SmallArray_Off_Len# (# SmallArray# x, I, I #)
type SmallMutableSlice ∷ ∀ {l}. ★ → T# l → K (# ByteArray#, I, I #)
newtype SmallMutableSlice s x = SmallMutableArray_Off_Len# (# SmallMutableArray# s x, I, I #)

type ConstRef ∷ ∀ {l}. T# l → K (# ByteArray#, I #)
newtype ConstRef x = Array_Off# (# Array# x, I #)
type SmallConstRef ∷ ∀ {l}. T# l → K (# ByteArray#, I #)
newtype SmallConstRef x = SmallArray_Off# (# SmallArray# x, I #)
type Ref ∷ ∀ {l}. ★ → T# l → K (# ByteArray#, I #)
newtype Ref s x = MutableArray_Off# (# MutableArray# s x, I #)
type SmallRef ∷ ∀ {l}. ★ → T# l → K (# ByteArray#, I #)
newtype SmallRef s x = SmallMutableArray_Off# (# SmallMutableArray# s x, I #)

-- | An unboxed vector with offset and length.
type UnboxedSlice ∷ ∀ {r}. T r → K (# ByteArray#, I , I #)
newtype UnboxedSlice x = Bytes_Off_Len# (# ByteArray# , I , I #)
type UnboxedMutableSlice ∷ ∀ {r}. ★ → T r → K (# ByteArray#, I , I #)
newtype UnboxedMutableSlice s x = MBytes_Off_Len# (# MutableByteArray# s, I , I #)
type PinnedSlice ∷ ∀ {r}. T r → K (# ByteArray#, I , I #)
newtype PinnedSlice x = PinnedBytes_Off_Len# (# ByteArray# , I , I #)
type PinnedMutableSlice ∷ ∀ {r}. ★ → T r → K (# ByteArray#, I , I #)
newtype PinnedMutableSlice s x = PinnedMBytes_Off_Len# (# MutableByteArray# s, I , I #)

type UnboxedConstRef ∷ ∀ {r}. T r → K (# ByteArray#, I #)
newtype UnboxedConstRef x = Bytes_Off# (# ByteArray#, I #)
type UnboxedRef ∷ ∀ {r}. ★ → T r → K (# ByteArray#, I #)
newtype UnboxedRef s x = MBytes_Off# (# MutableByteArray# s, I #)

type PinnedConstRef ∷ ∀ {r}. T r → K (# ByteArray#, I #)
newtype PinnedConstRef x = PinnedBytes_Off# (# ByteArray#, I #)
type PinnedRef ∷ ∀ {r}. ★ → T r → K (# ByteArray#, I #)
newtype PinnedRef s x = MPinnedBytes_Off# (# MutableByteArray# s, I #)

type ForeignSlice ∷ ∀ {r}. T r → K (# Addr#, I #)
newtype ForeignSlice x = Addr_Len# (# Addr#, I #)
type ForeignMutableSlice ∷ ∀ {r}. ★ → T r → K (# Addr#, I #)
newtype ForeignMutableSlice s x = MAddr_Len# (# Addr#, I #)


type UnboxedArray# ∷ ∀ {r}. T r → T_
newtype UnboxedArray# x = ByteArray# ByteArray#

type UnboxedMutableArray# ∷ ∀ {r}. ★ → T r → T_
newtype UnboxedMutableArray# s x = MutableByteArray# (MutableByteArray# s)

-- | (Possibly heterogeneous) contiguous bytes.
-- Pinned to an address and gaurenteed not to be moved by GC.
type PinnedArray# ∷ ∀ {r}. T r → T_
newtype PinnedArray# x = PinnedByteArray# ByteArray#

type PinnedMutableArray# ∷ ∀ {r}. ★ → T r → T_
newtype PinnedMutableArray# s x = PinnedMutableByteArray# (MutableByteArray# s)

type A ∷ ∀ {rs} {r}. T rs → T r → T_
-- | Primitive array type.
-- The concrete representation can be determined by the kind of its contents
type family A (s ∷ T rs) (x ∷ T r) = a | a → rs r where
  A X (x ∷ T# _) = Array# x
  A X (x ∷ K I) = UnboxedArray# x
  A X (x ∷ K I1) = UnboxedArray# x
  A X (x ∷ K I2) = UnboxedArray# x
  A X (x ∷ K I4) = UnboxedArray# x
  A X (x ∷ K I8) = UnboxedArray# x
  A X (x ∷ K U) = UnboxedArray# x
  A X (x ∷ K U1) = UnboxedArray# x
  A X (x ∷ K U2) = UnboxedArray# x
  A X (x ∷ K U4) = UnboxedArray# x
  A X (x ∷ K U8) = UnboxedArray# x
  A X (x ∷ K F4) = UnboxedArray# x
  A X (x ∷ K F8) = UnboxedArray# x
  A X (x ∷ K Addr#) = UnboxedArray# x
  A s (x ∷ T# _) = MutableArray# s x
  A s (x ∷ K I) = UnboxedMutableArray# s x
  A s (x ∷ K I1) = UnboxedMutableArray# s x
  A s (x ∷ K I2) = UnboxedMutableArray# s x
  A s (x ∷ K I4) = UnboxedMutableArray# s x
  A s (x ∷ K I8) = UnboxedMutableArray# s x
  A s (x ∷ K U) = UnboxedMutableArray# s x
  A s (x ∷ K U1) = UnboxedMutableArray# s x
  A s (x ∷ K U2) = UnboxedMutableArray# s x
  A s (x ∷ K U4) = UnboxedMutableArray# s x
  A s (x ∷ K U8) = UnboxedMutableArray# s x
  A s (x ∷ K F4) = UnboxedMutableArray# s x
  A s (x ∷ K F8) = UnboxedMutableArray# s x
  A s (x ∷ K Addr#) = UnboxedMutableArray# s x

type M ∷ ∀ {ra} {r}. (T ra → T r) → ★ → T ra → T r
type family M a = ma | ma → a where
  M UnboxedArray# = UnboxedMutableArray#
  M PinnedArray# = PinnedMutableArray#
  M SmallArray# = SmallMutableArray#
  M Array# = MutableArray#
  M ForeignArray# = ForeignMutableArray#

-- | A C-style null-terminated string of Latin-1 @Char1#@ or UTF-8 @Char#@
type S# ∷ Encoding → K Addr#
newtype S# a = S# Addr#

data Encoding = Latin1 | UTF8

-- | An machine address to valid data, assumed to point outside the garbage-collected heap
type ForeignArray# ∷ ∀ {r}. T r → K Addr#
newtype ForeignArray# x = ConstAddr# Addr#
type ForeignMutableArray# ∷ ∀ {r}. ★ → T r → K Addr#
newtype ForeignMutableArray# s x = Addr# Addr#


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

-- | The uninhabited ("Void") type
newtype X ∷ T (SumRep '[]) where X ∷ X → X

type Box ∷ ∀ {r}. T r → ★
type family Box x = b | b → x where
  Box I = GHC.Int
  Box I1 = GHC.Int8
  Box I2 = GHC.Int16
  Box I4 = GHC.Int32
  Box I8 = GHC.Int64
  Box U = GHC.Word
  Box U1 = GHC.Word8
  Box U2 = GHC.Word16
  Box U4 = GHC.Word32
  Box U8 = GHC.Word64
  Box Ordering = GHC.Ordering
  Box B# = GHC.Bool
  Box Nat = BigNat


type VRep ∷ ∀ {r}. T r → Natural → RuntimeRep
type family VRep v n = r | r → v n where
  VRep I1  16 = R Int8X16#
  VRep I2 8  = R Int16X8#
  VRep I4 4  = R Int32X4#
  VRep I8 2  = R Int64X2#

  VRep I1  32 = R Int8X32#
  VRep I2 16 = R Int16X16#
  VRep I4 8  = R Int32X8#
  VRep I8 4  = R Int64X4#

  VRep I1 64  = R Int8X64#
  VRep I2 32 = R Int16X32#
  VRep I4 16 = R Int32X16#
  VRep I8 8  = R Int64X8#


  VRep U1  16 = R Word8X16#
  VRep U2 8  = R Word16X8#
  VRep U4 4  = R Word32X4#
  VRep U8 2  = R Word64X2#

  VRep U1  32 = R Word8X32#
  VRep U2 16 = R Word16X16#
  VRep U4 8  = R Word32X8#
  VRep U8 4  = R Word64X4#

  VRep U1  64 = R Word8X64#
  VRep U2 32 = R Word16X32#
  VRep U4 16 = R Word32X16#
  VRep U8 8  = R Word64X8#


  VRep F4 4  = R FloatX4#
  VRep F8 2  = R DoubleX2#

  VRep F4 8  = R FloatX8#
  VRep F8 4  = R DoubleX4#

  VRep F4 16 = R FloatX16#
  VRep F8 8  = R DoubleX8#

type VElem ∷ ∀ {r}. T r → VecElem
type family VElem a = e | e → a where
  VElem I1 = Int8ElemRep
  VElem I2 = Int16ElemRep
  VElem I4 = Int32ElemRep
  VElem I8 = Int64ElemRep
  VElem U1 = Word8ElemRep
  VElem U2 = Word16ElemRep
  VElem U4 = Word32ElemRep
  VElem U8 = Word64ElemRep
  VElem F4 = FloatElemRep
  VElem F8 = DoubleElemRep
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
  I1  × 16 = Int8X16#
  I2 ×  8 = Int16X8#
  I4 ×  4 = Int32X4#
  I8 ×  2 = Int64X2#

  I1  × 32 = Int8X32#
  I2 × 16 = Int16X16#
  I4 ×  8 = Int32X8#
  I8 ×  4 = Int64X4#

  I1  × 64 = Int8X64#
  I2 × 32 = Int16X32#
  I4 × 16 = Int32X16#
  I8 ×  8 = Int64X8#


  U1  × 16 = Word8X16#
  U2 ×  8 = Word16X8#
  U4 ×  4 = Word32X4#
  U8 ×  2 = Word64X2#

  U1  × 32 = Word8X32#
  U2 × 16 = Word16X16#
  U4 ×  8 = Word32X8#
  U8 ×  4 = Word64X4#

  U1  × 64 = Word8X64#
  U2 × 32 = Word16X32#
  U4 × 16 = Word32X16#
  U8 ×  8 = Word64X8#


  F4 ×  4 = FloatX4#
  F8 ×  2 = DoubleX2#

  F4 ×  8 = FloatX8#
  F8 ×  4 = DoubleX4#

  F4 × 16 = FloatX16#
  F8 ×  8 = DoubleX8#

-- | A trivially satisfied 'Constraint'
type OK ∷ ∀ {r}. T r → Constraint
class OK a
instance OK (a ∷ T r)
