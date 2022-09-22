{-# language CPP #-}
module Array.Index where
import Prim
import Memset
import qualified GHC.Types as GHC
import Action


-- | Operations for containers of contiguous primitive values.
type (∈) ∷ ∀ {r} {ra}. T r → (T r → T ra) → Constraint
class x ∈ a where
  -- | Index an element. 
  (!) ∷ a x → I {- ^ Offset in elements -} → x
  -- | Read an element.
  (!!) ∷ M a s x → I → ST s x
  -- | Write an element. 
  write ∷ M a s x → I → x → ST_ s
  -- | Set all elements.
  set# ∷ M a s x → I {- ^ offset -} → I {- ^ # elements to set -} → x → ST_ s

-- | "A.Box".
--
-- @index#@ Forces the indexing but not the value. For more laziness use 'A.Box.indexLazy#'
--
-- @new@ uses sharing
instance x ∈ Array# where
  write = writeArray#
  (!!) = readArray#
  (!) a i = case indexArray# a i of (# a #) → a
  set# a off n x = go 0# where
    go i = case i >=# n of 0# → \s → go (i +# 1#) (write a i x s)
                           1# → \s→s
instance (x ∷ T_) ∈ Array# where
  write = writeArray#
  (!!) = readArray#
  (!) a i = case indexArray# a i of (# a #) → a
  set# a off n x = go 0# where
    go i = case i >=# n of 0# → \s → go (i +# 1#) (write a i x s)
                           1# → \s→s

-- | "A.Boxed.Small".
--
-- @index#@ Forces the indexing but not the value. For more laziness use 'A.Box.Small.indexLazy#'
--
-- @new@ uses sharing
instance x ∈ SmallArray# where
  write = writeSmallArray#
  (!!) = readSmallArray# 
  (!) a i = case indexSmallArray# a i of (# a #) → a
  set# a off n x = go 0# where
    go i = case i >=# n of 0# → \s → go (i +# 1#) (write a i x s)
                           1# → \s→s
instance (x ∷ T_) ∈ SmallArray# where
  write = writeSmallArray# 
  (!!) = readSmallArray# 
  (!) a i = case indexSmallArray# a i of (# a #) → a
  set# a off n x = go 0# where
    go i = case i >=# n of 0# → \s → go (i +# 1#) (write a i x s)
                           1# → \s→s

#define INST_IN(TY,TA,IX,RD,WR,SET) \
instance (x ≑ TY) ⇒ x ∈ (TA) where {\
  (!) = coerce IX#; \
  (!!) = coerce RD# ; \
  write = coerce WR# ; \
  set# a i n x = unio (SET# (coerce a) i n (coerce x))}
#define INST_IN_SPEC(TY,TA,IX,RD,WR,SET) \
instance {-# OVERLAPPING #-} (TY) ∈ (TA) where {\
  (!) = coerce IX#; \
  (!!) = coerce RD# ; \
  write = coerce WR# ; \
  set# a i n x = unio (SET# (coerce a) i n (coerce x))}

INST_IN(I,UnboxedArray#,indexIntArray,readIntArray,writeIntArray,setIntArray)
INST_IN(I,ForeignArray#,indexIntOffAddr,readIntOffAddr,writeIntOffAddr,setIntOffAddr)
INST_IN(I8,UnboxedArray#,indexInt8Array,readInt8Array,writeInt8Array,setInt8Array)
INST_IN(I8,ForeignArray#,indexInt8OffAddr,readInt8OffAddr,writeInt8OffAddr,setInt8OffAddr)
INST_IN(I16,UnboxedArray#,indexInt16Array,readInt16Array,writeInt16Array,setInt16Array)
INST_IN(I16,ForeignArray#,indexInt16OffAddr,readInt16OffAddr,writeInt16OffAddr,setInt16OffAddr)
INST_IN(I32,UnboxedArray#,indexInt32Array,readInt32Array,writeInt32Array,setInt32Array)
INST_IN(I32,ForeignArray#,indexInt32OffAddr,readInt32OffAddr,writeInt32OffAddr,setInt32OffAddr)
INST_IN(I64,UnboxedArray#,indexInt64Array,readInt64Array,writeInt64Array,setInt64Array)
INST_IN(I64,ForeignArray#,indexInt64OffAddr,readInt64OffAddr,writeInt64OffAddr,setInt64OffAddr)
INST_IN(U,UnboxedArray#,indexWordArray,readWordArray,writeWordArray,setWordArray)
INST_IN(U,ForeignArray#,indexWordOffAddr,readWordOffAddr,writeWordOffAddr,setWordOffAddr)
INST_IN(U8,UnboxedArray#,indexWord8Array,readWord8Array,writeWord8Array,setWord8Array)
INST_IN(U8,ForeignArray#,indexWord8OffAddr,readWord8OffAddr,writeWord8OffAddr,setWord8OffAddr)
INST_IN(U16,UnboxedArray#,indexWord16Array,readWord16Array,writeWord16Array,setWord16Array)
INST_IN(U16,ForeignArray#,indexWord16OffAddr,readWord16OffAddr,writeWord16OffAddr,setWord16OffAddr)
INST_IN(U32,UnboxedArray#,indexWord32Array,readWord32Array,writeWord32Array,setWord32Array)
INST_IN(U32,ForeignArray#,indexWord32OffAddr,readWord32OffAddr,writeWord32OffAddr,setWord32OffAddr)
INST_IN(U64,UnboxedArray#,indexWord64Array,readWord64Array,writeWord64Array,setWord64Array)
INST_IN(U64,ForeignArray#,indexWord64OffAddr,readWord64OffAddr,writeWord64OffAddr,setWord64OffAddr)
INST_IN_SPEC(Char8#,UnboxedArray#,indexCharArray,readCharArray,writeCharArray,setCharArray)
INST_IN_SPEC(Char8#,ForeignArray#,indexCharOffAddr,readCharOffAddr,writeCharOffAddr,setCharOffAddr)
INST_IN_SPEC(Char#,UnboxedArray#,indexWideCharArray,readWideCharArray,writeWideCharArray,setWideCharArray)
INST_IN_SPEC(Char#,ForeignArray#,indexWideCharOffAddr,readWideCharOffAddr,writeWideCharOffAddr,setWideCharOffAddr)
INST_IN(Addr#,UnboxedArray#,indexAddrArray,readAddrArray,writeAddrArray,setAddrArray)
INST_IN(Addr#,ForeignArray#,indexAddrOffAddr,readAddrOffAddr,writeAddrOffAddr,setAddrOffAddr)




{-
INST_UNBOX(I)
INST_UNBOX(I8)
INST_UNBOX(I16)
INST_UNBOX(I32)
INST_UNBOX(I64)
INST_UNBOX(U)
INST_UNBOX(U8)
INST_UNBOX(U16)
INST_UNBOX(U32)
INST_UNBOX(U64)
INST_UNBOX(Char)
INST_UNBOX(Char8)
INST_UNBOX(Addr#)
-- INST_UNBOX((P_Stable s))
-}

-- | Bit indexing (no @read#@ or @write@)
--instance B ∈ U where index# u i = B# (geWord# (and# u (uncheckedShiftL# 1## (word2Int# u))) 1## )

unio ∷ GHC.IO () → ST_ s
unio (GHC.IO io) s = case unsafeCoerce# io s of (# s' , _ #) → s'
{-# INLINE unio #-}
