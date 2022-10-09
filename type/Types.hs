{-# language CPP,NoImplicitPrelude #-}
module Types (Bool(F,T), B(F#,T#), module Types, module X) where
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
newtype Nat = BigNat# (A' U)


-- | The kind constructor of types abstracted over 'RuntimeRep'
type T = TYPE
-- | The kind of an unlifted type
type T_ = TYPE (BoxedRep Unlifted)
-- | The kind of types with no runtime representation
type T0 = TYPE (TupleRep '[])
-- | The kind constructor of boxed types
type T# l = TYPE (BoxedRep l)
-- | The kind of constraints
type TC = Constraint

-- | The kind of a type
type K (a ∷ k) = k
-- | The 'RuntimeRep' of a type
type R (i ∷ T r) = r

-- | Unlifted boolean values 'F#' and 'T#' represented by machine integers
newtype B = B# I
pattern F#, T# ∷ B
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


-- | The character type Char is an enumeration whose values represent Unicode (or equivalently ISO/IEC 10646) code points (i.e. characters, see http://www.unicode.org/ for details). This set extends the ISO 8859-1 (Latin-1) character set (the first 256 characters), which is itself an extension of the ASCII character set (the first 128 characters). An unboxed character literal @'c'#@ has type 'C#'
type C = Char#
-- | 8-bit Latin-1 code points (the first 256 characters of 'C#'). Can be safely upcast into 'C#' via 'coerce', and safely downcast from 'C#' if in the range @0-255@
newtype C1 = C1# C

-- | 8-bit Latin-1 code points (the first 256 characters of 'Char'). Can be safely upcast into 'Char' via 'coerce', and safely downcast from 'Char' if in the range @0-255@
newtype Char8 = Char8# Char

-- | Machine-sized signed integers
type I = Int#
-- | Single-byte (8 bit) signed integers
type I1 = Int8#
-- | Narrow a machine 'I' to 8 bits
pattern I1 ∷ I → I1
pattern I1 u ← (int8ToInt# → u) where I1 = intToInt8#
{-# complete I1 #-}

-- | 2-byte (16 bit) signed integers
type I2 = Int16#
-- | Narrow a machine 'I' to 16 bits
pattern I2 ∷ I → I2
pattern I2 u ← (int16ToInt# → u) where I2 = intToInt16#
{-# complete I2 #-}

-- | 4-byte (32 bit) signed integers
type I4 = Int32#
-- | Narrow a machine 'I' to 32 bits
pattern I4 ∷ I → I4
pattern I4 u ← (int32ToInt# → u) where I4 = intToInt32#
{-# complete I4 #-}

-- | 8-byte (64 bit) signed integers
type I8 = Int64#
-- | Convert a machine 'I'
pattern I8 ∷ I → I8
pattern I8 u ← (int64ToInt# → u) where I8 = intToInt64#
{-# complete I8 #-}

-- | Machine-sized unsigned integers
type U = Word#

-- | Single-byte (8 bit) unsigned integers
type U1 = Word8#
-- | Narrow a machine 'U' to 8 bits
pattern U1 ∷ U → U1
pattern U1 u ← (word8ToWord# → u) where U1 = wordToWord8#
{-# complete U1 #-}

-- | 2-byte (16 bit) unsigned integers
type U2 = Word16#

-- | Narrow a machine 'U' to 16 bits
pattern U2 ∷ U → U2
pattern U2 u ← (word16ToWord# → u) where U2 = wordToWord16#
{-# complete U2 #-}

-- | 4-byte (32 bit) unsigned integers
type U4 = Word32#
-- | Narrow a machine 'U' to 32 bits
pattern U4 ∷ U → U4
pattern U4 u ← (word32ToWord# → u) where U4 = wordToWord32#
{-# complete U4 #-}

-- | 8-byte (64 bit) unsigned integers
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

-- | Stateful computations (in state thread @s@) returning values of type @a@
type ST s (a ∷ T ra) = State# s → (# State# s , a #)
-- | Stateful computations (in state thread @s@) possibly returning values of type @a@.
-- If the boolean is 'F#', the @a@ value is invalid and should not be used.
type ST' s (a ∷ T ra) = State# s → (# State# s , B, a #)
-- | Stateful computations (in state thread @s@) that do not return a value.
type ST_ s = State# s → State# s

-- | A computation performing some I\/O before returning a value of type @a@.
type IO (a ∷ T r)  = ST RealWorld a
-- | A computation performing some I\/O before possibly returning a value of type @a@.
-- If the boolean is 'F#', the @a@ value is invalid and should not be used.
type IO' (a ∷ T ra) = State# RealWorld → (# State# RealWorld , B, a #)
-- | A computation performing some I\/O
type IO_ = ST_ RealWorld

-- * Transactional Memory Operations
data Transaction

-- | An atomic (transactional) operation on shared memory, returning a value of type @a@
type STM (a ∷ T r) = ST Transaction a
-- | An atomic (transactional) operation on shared memory
type STM_ = ST_ Transaction

-- | A mutable array of boxed (lifted or unlifted) values.
type Ar ∷ ∀ {l}. ★ → T# l → T_
type Ar = SmallMutableArray#
-- | A constant array of boxed (lifted or unlifted) values.
type Ar' ∷ ∀ {l}. T# l → T_
type Ar' = SmallArray#
-- | A big mutable array of boxed (lifted or unlifted) values.
-- This marks segments the array of size 128 during garbage collection for
-- better GC performance on small mutations to large arrays,
-- at the cost of a small amount of memory (for the card table) and write speed (to update the card table)
type AR ∷ ∀ {l}. ★ → T# l → T_
type AR = MutableArray#
-- | A big constant array of boxed (lifted or unlifted) values.
type AR' ∷ ∀ {l}. T# l → T_
type AR' = Array#

-- | An immutable array of packed bytes representing values of type @x@
type A' ∷ ∀ {r}. T r → T_
newtype A' x = A'# ByteArray#

-- | A mutable array (in state thread @s@) of packed bytes representing values of type @x@
type A ∷ ∀ {r}. ★ → T r → T_
newtype A s x = A# (MutableByteArray# s)

-- | An immutable array of packed bytes representing values of type @x@
-- Pinned to an address and gaurenteed not to be moved by GC.
type A'_ ∷ ∀ {r}. T r → T_
newtype A'_ x = A'_# ByteArray#

-- | A mutable array (in state thread @s@) of packed bytes representing values of type @x@
-- Pinned to an address and gaurenteed not to be moved by GC.
type A_ ∷ ∀ {r}. ★ → T r → T_
newtype A_ s x = A_# (MutableByteArray# s)

-- | Mutible version 
type M ∷ ∀ {ra} {r}. (T ra → T r) → ★ → T ra → T r
type family M a = ma | ma → a where
  M A' = A
  M A'# = A#
  M A'## = A##
  M A'_ = A_
  M A'_# = A_#
  M A'_## = A_##
  M Ar' = Ar
  M Ar'# = Ar#
  M Ar'## = Ar##
  M AR' = AR
  M AR'# = AR#
  M AR'## = AR##
  M P' = P
  M P'## = P##


-- | A slice into an 'Array#'
type AR'## ∷ ∀ {l}. T# l → K (# ByteArray#, I, I #)
newtype AR'## x = AR'_Off_Len# (# AR' x, I, I #)
-- | A slice into a 'MutableArray#'
type AR## ∷ ∀ {l}. ★ → T# l → K (# ByteArray#, I, I #)
newtype AR## s x = Arr_Off_Len# (# AR s x, I, I #)
-- | A slice into a 'Ar\''
type Ar'## ∷ ∀ {l}. T# l → K (# ByteArray#, I, I #)
newtype Ar'## x = Ar'_Off_Len# (# Ar' x, I, I #)
-- | A slice into a 'Ar'
type Ar## ∷ ∀ {l}. ★ → T# l → K (# ByteArray#, I, I #)
newtype Ar## s x = Ar_Off_Len# (# Ar s x, I, I #)

-- | Reference into a single value of type @x@ of an 'Array#'
type AR'# ∷ ∀ {l}. T# l → K (# ByteArray#, I #)
newtype AR'# x = Array_Off# (# AR' x, I #)
-- | Reference into a single value of type @x@ of a 'SmallArray#'
type Ar'# ∷ ∀ {l}. T# l → K (# ByteArray#, I #)
newtype Ar'# x = SmallArray_Off# (# Ar' x, I #)
-- | Reference into a single value of type @x@ of a 'MutableArray#'
type AR# ∷ ∀ {l}. ★ → T# l → K (# ByteArray#, I #)
newtype AR# s x = MutableArray_Off# (# AR s x, I #)
-- | Reference into a single value of type @x@ of a 'Ar'
type Ar# ∷ ∀ {l}. ★ → T# l → K (# ByteArray#, I #)
newtype Ar# s x = Ar_Off# (# Ar s x, I #)

-- | A slice into a value of type @x@ in a 'A''
type A'## ∷ ∀ {r}. T r → K (# ByteArray#, I , I #)
newtype A'## x = Bytes_Off_Len# (# ByteArray# , I , I #)
-- | A slice (in state thread @s@) into a value of type @x@ in a 'A'
type A## ∷ ∀ {r}. ★ → T r → K (# ByteArray#, I , I #)
newtype A## s x = MBytes_Off_Len# (# MutableByteArray# s, I , I #)
-- | A slice into a value of type @x@ in a 'A'_'
type A'_## ∷ ∀ {r}. T r → K (# ByteArray#, I , I #)
newtype A'_## x = PinnedBytes_Off_Len# (# ByteArray# , I , I #)
-- | A slice (in state thread @s@) into a value of type @x@ in a 'A_'
type A_## ∷ ∀ {r}. ★ → T r → K (# ByteArray#, I , I #)
newtype A_## s x = PinnedMBytes_Off_Len# (# MutableByteArray# s, I , I #)

-- | Reference into a single value of type @x@ of an 'A''
type A'# ∷ ∀ {r}. T r → K (# ByteArray#, I #)
newtype A'# x = Bytes_Off# (# ByteArray#, I #)
-- | Reference (in state thread @s@) into a single value of type @x@ of an 'A'
type A# ∷ ∀ {r}. ★ → T r → K (# ByteArray#, I #)
newtype A# s x = MBytes_Off# (# MutableByteArray# s, I #)

-- | Reference into a single value of type @x@ of a 'A'_'
type A'_# ∷ ∀ {r}. T r → K (# ByteArray#, I #)
newtype A'_# x = PinnedBytes_Off# (# ByteArray#, I #)
-- | Reference (in state thread @s@) into a single value of type @x@ in a 'A_'
type A_# ∷ ∀ {r}. ★ → T r → K (# ByteArray#, I #)
newtype A_# s x = MPinnedBytes_Off# (# MutableByteArray# s, I #)

-- | An immutable array backed by unmanaged 'P' memory
type P'## ∷ ∀ {r}. T r → K (# Addr#, I #)
newtype P'## x = Addr_Len# (# Addr#, I #)
-- | A mutable array (in state thread @s@) backed by unmanaged 'P\'' memory
type P## ∷ ∀ {r}. ★ → T r → K (# Addr#, I #)
newtype P## s x = MAddr_Len# (# Addr#, I #)

-- | A C-style null-terminated string of Latin-1 @C1#@ or UTF-8 @C#@
type S ∷ ∀ {r}. T r → K Addr#
newtype S a = S# Addr#

-- | Constant machine address to valid data, assumed to point outside the garbage-collected heap
type P' ∷ ∀ {r}. T r → K Addr#
newtype P' x = ConstAddr# Addr#
-- | Constant machine address to valid data, assumed to point outside the garbage-collected heap
type P ∷ ∀ {r}. ★ → T r → K Addr#
newtype P s x = Addr# Addr#


-- | Shared memory locations that support atomic memory transactions.
newtype Async# x = TVar# (TVar# Transaction x)
-- | A synchronising variable, used for communication between concurrent threads.
-- It can be thought of as a box, which may be empty or full.
--
-- The RTS implementation is really an abstraction for
-- connecting 'take' and 'write' calls between threads
type Sync# = MVar# RealWorld

-- | A mutable reference to a _lifted_ value
type ST# = MutVar#

{- |
A /stable pointer/ is a reference to a Haskell expression that is
guaranteed not to be affected by garbage collection, i.e., it will neither be
deallocated nor will the value of the stable pointer itself change during
garbage collection (ordinary references may be relocated during garbage
collection).  Consequently, stable pointers can be passed to foreign code,
which can treat it as an opaque reference to a Haskell value.
-}
type Stable# = StablePtr#

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
  Box B = GHC.Bool
  Box Nat = BigNat


type VRep ∷ ∀ {r}. T r → Natural → RuntimeRep
type family VRep v n = r | r → v n where
  VRep I1 16 = R Int8X16#
  VRep I2 8  = R Int16X8#
  VRep I4 4  = R Int32X4#
  VRep I8 2  = R Int64X2#

  VRep I1 32 = R Int8X32#
  VRep I2 16 = R Int16X16#
  VRep I4 8  = R Int32X8#
  VRep I8 4  = R Int64X4#

  VRep I1 64  = R Int8X64#
  VRep I2 32 = R Int16X32#
  VRep I4 16 = R Int32X16#
  VRep I8 8  = R Int64X8#


  VRep U1 16 = R Word8X16#
  VRep U2 8  = R Word16X8#
  VRep U4 4  = R Word32X4#
  VRep U8 2  = R Word64X2#

  VRep U1 32 = R Word8X32#
  VRep U2 16 = R Word16X16#
  VRep U4 8  = R Word32X8#
  VRep U8 4  = R Word64X4#

  VRep U1 64 = R Word8X64#
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
  I1 × 16 = Int8X16#
  I2 ×  8 = Int16X8#
  I4 ×  4 = Int32X4#
  I8 ×  2 = Int64X2#

  I1 × 32 = Int8X32#
  I2 × 16 = Int16X16#
  I4 ×  8 = Int32X8#
  I8 ×  4 = Int64X4#

  I1 × 64 = Int8X64#
  I2 × 32 = Int16X32#
  I4 × 16 = Int32X16#
  I8 ×  8 = Int64X8#


  U1 × 16 = Word8X16#
  U2 ×  8 = Word16X8#
  U4 ×  4 = Word32X4#
  U8 ×  2 = Word64X2#

  U1 × 32 = Word8X32#
  U2 × 16 = Word16X16#
  U4 ×  8 = Word32X8#
  U8 ×  4 = Word64X4#

  U1 × 64 = Word8X64#
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
