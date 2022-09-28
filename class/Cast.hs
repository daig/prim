{-# language CPP #-}
module Cast where
import Do.ST as ST
import Unsafe.Coerce
import GHC.Types (isTrue#,Bool,Char(..))
import GHC.Int
import GHC.Word
import GHC.Prim qualified as GHC
import GHC.CString
import {-# source #-} Bits
import {-# source #-} Num
import Prim

-- | Nontrivial but conceptually-unique conversions between types. Use with care!
type Cast ∷ forall {r ∷ RuntimeRep} {r' ∷ RuntimeRep}. T r → T r' → C
class Cast b a where cast ∷ a → b

-- | Numeric conversion
instance Cast I U where cast = word2Int#
-- | Numeric conversion
instance Cast F32 U where cast = word2Float#
-- | Numeric conversion
instance Cast F64 U where cast = word2Double#

-- | Numeric conversion
instance Cast U I where cast = int2Word#
-- | Numeric conversion
instance Cast F32 I where cast = int2Float#
-- | Numeric conversion
instance Cast F64 I where cast = int2Double#

-- | Truthiness
instance Cast B# I where cast = coerce (0# >#)
-- | Truthiness
instance Cast B# I8 where cast = coerce (gtInt8# (cast 0#))
-- | Truthiness
instance Cast B# I16 where cast = coerce (gtInt16# (cast 0#))
-- | Truthiness
instance Cast B# I32 where cast = coerce (gtInt32# (cast 0#))
-- | Truthiness
instance Cast B# I64 where cast = coerce (gtInt64# (cast 0#))
-- | Truthiness
instance Cast B# U where cast = coerce do gtWord# 0##
-- | Truthiness
instance Cast B# U8 where cast = coerce (gtWord8# (cast 0##))
-- | Truthiness
instance Cast B# U16 where cast = coerce (gtWord16# (cast 0##))
-- | Truthiness
instance Cast B# U32 where cast = coerce (gtWord32# (cast 0##))
-- | Truthiness
instance Cast B# U64 where cast = coerce (gtWord64# (cast 0##))
-- | Truthiness
instance Cast B# Addr# where cast = coerce neAddr# nullAddr#
-- | Truthiness
instance Cast B# Char# where cast = coerce neChar# '\NUL'#
-- | Truthiness
deriving via Char# instance Cast B# Char8#

instance Cast I B# where cast (B# i#) = i#
instance Cast I8 B# where cast (B# i#) = cast i#
instance Cast I16 B# where cast (B# i#) = cast i#
instance Cast I32 B# where cast (B# i#) = cast i#
instance Cast I64 B# where cast (B# i#) = cast i#
instance Cast U B# where cast (B# i#) = cast i#
instance Cast U8 B# where cast (B# i#) = cast (cast @U i#)
instance Cast U16 B# where cast (B# i#) = cast (cast @U i#)
instance Cast U32 B# where cast (B# i#) = cast (cast @U i#)
instance Cast U64 B# where cast (B# i#) = cast (cast @U i#)

-- | Numeric conversion
instance Cast F32 F64 where cast = double2Float#
-- | Numeric conversion
instance Cast F64 F32 where cast = float2Double#
-- | Truncate ie round towards zero
instance Cast I F32 where cast = float2Int#
-- | Truncate ie round towards zero
instance Cast I F64 where cast = double2Int#

-- | Numeric conversion via extension
instance Cast I I8 where cast = int8ToInt#
-- | Numeric conversion via extension
instance Cast I I16 where cast = int16ToInt#
-- | Numeric conversion via extension
instance Cast I I32 where cast = int32ToInt#
-- | Numeric conversion. Should be NOP
instance Cast I I64 where cast = int64ToInt#
-- | Numeric conversion via truncation
instance Cast I8 I where cast = intToInt8#
-- | Numeric conversion via truncation
instance Cast I16 I where cast = intToInt16#
-- | Numeric conversion via triuncation
instance Cast I32 I where cast = intToInt32#
-- | Numeric conversion. Should be NOP
instance Cast I64 I where cast = intToInt64#

-- | Numeric conversion via extension
instance Cast I U8 where cast u = word2Int# (word8ToWord# u)
-- | Numeric conversion via extension
instance Cast I U16 where cast u = word2Int# (word16ToWord# u)
-- | Numeric conversion via extension
instance Cast I U32 where cast u = word2Int# (word32ToWord# u)
-- | Numeric conversion. Should be NOP
instance Cast I U64 where cast u = word2Int# (word64ToWord# u)

-- | Numeric conversion. Should be NOP
instance Cast I8 U8 where cast = word8ToInt8#
-- | Numeric conversion. Should be NOP
instance Cast I16 U16 where cast = word16ToInt16#
-- | Numeric conversion. Should be NOP
instance Cast I32 U32 where cast = word32ToInt32#
-- | Numeric conversion. Should be NOP
instance Cast I64 U64 where cast = word64ToInt64#
-- | Numeric conversion. Should be NOP
instance Cast U8 I8 where cast = int8ToWord8#
-- | Numeric conversion. Should be NOP
instance Cast U16 I16 where cast = int16ToWord16#
-- | Numeric conversion. Should be NOP
instance Cast U32 I32 where cast = int32ToWord32#
-- | Numeric conversion. Should be NOP
instance Cast U64 I64 where cast = int64ToWord64#

-- | Numeric conversion via extension
instance Cast U U8 where cast = word8ToWord#
-- | Numeric conversion via extension
instance Cast U U16 where cast = word16ToWord#
-- | Numeric conversion via extension
instance Cast U U32 where cast = word32ToWord#
-- | Numeric conversion. Should be NOP
instance Cast U U64 where cast = word64ToWord#
-- | Numeric conversion via truncation
instance Cast U8 U where cast = wordToWord8#
-- | Numeric conversion via truncation
instance Cast U16 U where cast = wordToWord16#
-- | Numeric conversion via truncation
instance Cast U32 U where cast = wordToWord32#
-- | Numeric conversion. Should be NOP
instance Cast U64 U where cast = wordToWord64#

-- | Numeric conversion via extension
instance Cast U I8 where cast i = int2Word# (int8ToInt# i)
-- | Numeric conversion via extension
instance Cast U I16 where cast i = int2Word# (int16ToInt# i)
-- | Numeric conversion via extension
instance Cast U I32 where cast i = int2Word# (int32ToInt# i)
-- | Numeric conversion. Should be NOP
instance Cast U I64 where cast i = int2Word# (int64ToInt# i)


-- | Hash
instance Cast I (P_Stable_Name a) where cast = stableNameToInt#

-- | Extract (copy) the live portion of a 'UnboxedSlice'
instance Prim x ⇒ Cast (UnboxedArray# x) (UnboxedSlice x) where
  cast (Bytes_Off_Len# (# x, size @x → off, size @x → n #)) = runST ST.do
    mv <- newByteArray# n
    cast (copyByteArray# x off mv 0# n)
    v <- unsafeFreezeByteArray# mv
    return (coerce v)

-- | Extract (copy) the live portion of a 'UnboxedConstRef'
instance Prim x ⇒ Cast (UnboxedArray# x) (UnboxedConstRef x) where
  cast (Bytes_Off# (# x, size @x → off #)) = runST ST.do
    let n = sizeofByteArray# x -# off
    mv <- newByteArray# n
    cast (copyByteArray# x off mv 0# n)
    v <- unsafeFreezeByteArray# mv
    return (coerce v)

-- | Extract (copy) the live portion of a 'UnboxedRef'
instance Prim x ⇒ Cast (PinnedArray# x) (PinnedConstRef x) where
  cast (PinnedBytes_Off# (# x, size @x → off #)) = runST ST.do
    let n = sizeofByteArray# x -# off
    mv <- newByteArray# n
    cast (copyByteArray# x off mv 0# n)
    v <- unsafeFreezeByteArray# mv
    return (coerce v)

-- | Wrap (no copy) 'ByteArray#' in a full-size 'Buffer'
instance Prim x ⇒ Cast (UnboxedSlice x) (UnboxedArray# x) where
  cast x = Bytes_Off_Len# (# coerce x, 0#, sizeofByteArray# (coerce x) / size @x 1# #)

instance Cast Addr# ByteArray# where cast = byteArrayContents#
instance Cast Addr# (MutableByteArray# s) where cast = mutableByteArrayContents#
instance Cast (ForeignArray# x) (PinnedArray# x) where cast = coerce byteArrayContents#
instance Cast (ForeignMutableArray# s x) (PinnedMutableArray# s x) where cast = coerce mutableByteArrayContents#

instance Cast I Char# where cast = ord#
instance Cast I Char8# where cast = coerce ord#
instance Cast Char# I where cast = chr#

-- | This pattern is strongly deprecated
instance Cast Addr# I where cast = int2Addr#
-- | This pattern is strongly deprecated
instance Cast I Addr# where cast = addr2Int#

-- | Atomically run a transaction
instance Cast (IO a) (STM a) where cast = unsafeCoerce# (atomically# @a)

instance Cast Bool B# where cast = coerce isTrue#
instance Cast B# Bool where cast p = B# if p then 1# else 0#

-- | Convert a tag and a (possibly invalid) value into an unboxed '(?)'
-- | Convert a tag (_True_ if it's _Err_) and a value into an unboxed 'Result'.

#define INST_CAST_SUM(X)\
instance Cast (ST s (# (y ∷ K (##)) | (x ∷ K X)  #)) (ST' s x) where { ;\
  cast st = \s → case st s of (# s', coerce → t, x #) → (# s', unsafeCoerce# (# t +# 1#, x #) #)} ;\
instance Cast (ST s (# (x ∷ K X) | (y ∷ K (##))  #)) (ST' s x) where { ;\
  cast st = \s → case st s of (# s', coerce → t, x #) → (# s', unsafeCoerce# (# t +# 1#, x #) #)} ;\
instance Cast (IO (# (x ∷ K X) | (y ∷ K (##))  #)) (IO' x) where { ;\
  cast st = \s → case st s of (# s', coerce → t, x #) → (# s', unsafeCoerce# (# t +# 1#, x #) #)} ;\
instance Cast (ST s (# (x ∷ K X) | x  #)) (ST' s x) where { ;\
  cast st = \s → case st s of (# s', coerce → t, x #) → (# s', unsafeCoerce# (# t +# 1#, x #) #)} ;\
instance Cast (# (y ∷ K (##)) | (x ∷ K X) #) (# B#, x #) where { ;\
  cast (# coerce → t, x #) = unsafeCoerce# (# t +# 1#, x #) } ;\
instance Cast (# (x ∷ K X) | (y ∷ K (##)) #) (# B#, x #) where { ;\
  cast (# coerce → t, x #) = unsafeCoerce# (# t +# 1#, x #) } ;\
instance Cast (# (x ∷ K X) | x #) (# B#, x #) where { ;\
  cast (# coerce → t, x #) = unsafeCoerce# (# t +# 1#, x #) } ;\

INST_CAST_SUM(I)
INST_CAST_SUM(I8)
INST_CAST_SUM(I16)
INST_CAST_SUM(I32)
INST_CAST_SUM(I64)
INST_CAST_SUM(U)
INST_CAST_SUM(U8)
INST_CAST_SUM(U16)
INST_CAST_SUM(U32)
INST_CAST_SUM(U64)
INST_CAST_SUM(F32)
INST_CAST_SUM(F64)
INST_CAST_SUM(Addr#)
INST_CAST_SUM(())
INST_CAST_SUM((##))

instance Cast (ST s (##)) (ST_ s) where cast st = \s → (# st s, (##) #)
instance Cast (ST_ s) (ST s (##)) where cast st = \s → case st s of (# s', _ #) → s'

-- Check if Right value
#define INST_CAST_BOOL_EITHER2(X,Y)\
instance Cast B# (# X | Y #) where {cast (unsafeCoerce# → (# t #)) = B# (t GHC.-# 1#)}

#define INST_CAST_UNMAYBE(X)\
instance Cast (# B#, X #) (# X | (##) #) where {cast (unsafeCoerce# → (# t, x #)) = (# (B# (t -# 1#)), x #) } ;\
instance Cast (# B#, X #) (# (##) | X #) where {cast (unsafeCoerce# → (# t, x #)) = (# B# (t -# 1#), x #) } ;\
instance Cast (# B#, X #) (# X | X #) where {cast (unsafeCoerce# → (# t, x #)) = (# B# (t GHC.-# 1#), x #) }

INST_CAST_UNMAYBE(I)
INST_CAST_UNMAYBE(I8)
INST_CAST_UNMAYBE(I16)
INST_CAST_UNMAYBE(I32)
INST_CAST_UNMAYBE(I64)
INST_CAST_UNMAYBE(U)
INST_CAST_UNMAYBE(U8)
INST_CAST_UNMAYBE(U16)
INST_CAST_UNMAYBE(U32)
INST_CAST_UNMAYBE(U64)
INST_CAST_UNMAYBE(F32)
INST_CAST_UNMAYBE(F64)
INST_CAST_UNMAYBE(Addr#)
INST_CAST_UNMAYBE(())



#define INST_CAST_BOOL_EITHER1(A)\
INST_CAST_BOOL_EITHER2(A,I) ;\
INST_CAST_BOOL_EITHER2(A,I8) ;\
INST_CAST_BOOL_EITHER2(A,I16) ;\
INST_CAST_BOOL_EITHER2(A,I32) ;\
INST_CAST_BOOL_EITHER2(A,I64) ;\
INST_CAST_BOOL_EITHER2(A,U) ;\
INST_CAST_BOOL_EITHER2(A,U8) ;\
INST_CAST_BOOL_EITHER2(A,U16) ;\
INST_CAST_BOOL_EITHER2(A,U32) ;\
INST_CAST_BOOL_EITHER2(A,U64) ;\
INST_CAST_BOOL_EITHER2(A,F32) ;\
INST_CAST_BOOL_EITHER2(A,F64) ;\
INST_CAST_BOOL_EITHER2(A,Addr#) ;\
INST_CAST_BOOL_EITHER2(A,()) ;\
INST_CAST_BOOL_EITHER2(A,(##)) ;\
INST_CAST_BOOL_EITHER2(A,ByteArray#)

INST_CAST_BOOL_EITHER1(I)
INST_CAST_BOOL_EITHER1(I8)
INST_CAST_BOOL_EITHER1(I16)
INST_CAST_BOOL_EITHER1(I32)
INST_CAST_BOOL_EITHER1(I64)
INST_CAST_BOOL_EITHER1(U)
INST_CAST_BOOL_EITHER1(U8)
INST_CAST_BOOL_EITHER1(U16)
INST_CAST_BOOL_EITHER1(U32)
INST_CAST_BOOL_EITHER1(U64)
INST_CAST_BOOL_EITHER1(F32)
INST_CAST_BOOL_EITHER1(F64)
INST_CAST_BOOL_EITHER1(Addr#)
INST_CAST_BOOL_EITHER1(())
INST_CAST_BOOL_EITHER1((##))
INST_CAST_BOOL_EITHER1(ByteArray#)


instance Cast Int I where cast = I#
instance Cast Int8 I8 where cast = I8#
instance Cast Int16 I16 where cast = I16#
instance Cast Int32 I32 where cast = I32#
instance Cast Int64 I64 where cast = I64#
instance Cast Word U where cast = W#
instance Cast Word8 U8 where cast = W8#
instance Cast Word16 U16 where cast = W16#
instance Cast Word32 U32 where cast = W32#
instance Cast Word64 U64 where cast = W64#
instance Cast Char Char# where cast = C#
instance Cast Char Char8# where cast = coerce C#

-- | Unpack bytes until \null byte
instance Cast [Char] (S# UTF8) where cast = coerce unpackCStringUtf8#
-- | Unpack bytes until \null byte
instance Cast [Char] (S# Latin1) where cast = coerce unpackCString#


instance Cast (State# y) (State# x) where cast = unsafeCoerce#

-- | Compute the length of a NUL-terminated string.
--
-- This address must refer to immutable memory.
--
-- GHC includes a built-in rule for constant folding when the argument is a statically-known literal. That is, a core-to-core pass reduces the expression @cstringLength# "hello"#@ to the constant @5#@.
instance Cast I (S# Latin1) where cast = coerce cstringLength#
