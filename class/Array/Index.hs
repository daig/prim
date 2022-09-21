{-# language CPP #-}
module Array.Index where
import Prim
import Memset
import qualified GHC.Types as GHC


-- | Operations for containers of contiguous primitive values.
type (∈) ∷ ∀ {r} {ra}. T r → (T r → T ra) → Constraint
class x ∈ a where
  -- | Index an element. 
  (!) ∷ a x → I {- ^ Offset in elements -} → x
  -- | Read an element.
  (!!) ∷ M (a x) s → I → ST s x
  -- | Write an element. 
  write# ∷ M (a x) s → I → x → ST_ s
  -- | Set all elements.
  set# ∷ M (a x) s → I {- ^ offset -} → I {- ^ # elements to set -} → x → ST_ s

-- | "A.Box".
--
-- @index#@ Forces the indexing but not the value. For more laziness use 'A.Box.indexLazy#'
--
-- @new@ uses sharing
instance x ∈ A_Box where
  write# = coerce (writeArray# @_ @x)
  (!!) = coerce (readArray# @_ @x)
  (!) a i = case coerce (indexArray# @x) a i of (# a #) -> a
  set# a off n x = go 0# where
    go i = case i >=# n of 0# → \s → go (i +# 1#) (write# a i x s)
                           1# → \s→s
instance (x ∷ T_) ∈ A_Box where
  write# = coerce (writeArray# @_ @x)
  (!!) = coerce (readArray# @_ @x)
  (!) a i = case coerce (indexArray# @x) a i of (# a #) -> a
  set# a off n x = go 0# where
    go i = case i >=# n of 0# → \s → go (i +# 1#) (write# a i x s)
                           1# → \s→s

-- | "A.Boxed.Small".
--
-- @index#@ Forces the indexing but not the value. For more laziness use 'A.Box.Small.indexLazy#'
--
-- @new@ uses sharing
instance x ∈ A_Box_Small where
  write# = coerce (writeSmallArray# @_ @x)
  (!!) = coerce (readSmallArray# @_ @x)
  (!) a i = case coerce (indexSmallArray# @x) a i of (# a #) -> a
  set# a off n x = go 0# where
    go i = case i >=# n of 0# → \s → go (i +# 1#) (write# a i x s)
                           1# → \s→s
instance (x ∷ T_) ∈ A_Box_Small where
  write# = coerce (writeSmallArray# @_ @x)
  (!!) = coerce (readSmallArray# @_ @x)
  (!) a i = case coerce (indexSmallArray# @x) a i of (# a #) -> a
  set# a off n x = go 0# where
    go i = case i >=# n of 0# → \s → go (i +# 1#) (write# a i x s)
                           1# → \s→s

#define INST_IN(TY,TA,IX,RD,WR,SET) \
instance (x ≑ TY) ⇒ x ∈ (TA) where {\
  (!) = coerce IX#; \
  (!!) = coerce RD# ; \
  write# = coerce WR# ; \
  set# a i n x = unio (SET# (coerce a) i n (coerce x))}
#define INST_IN_SPEC(TY,TA,IX,RD,WR,SET) \
instance (TY) ∈ (TA) where {\
  (!) = coerce IX#; \
  (!!) = coerce RD# ; \
  write# = coerce WR# ; \
  set# a i n x = unio (SET# (coerce a) i n (coerce x))}

INST_IN(I,A_Unbox,indexIntArray,readIntArray,writeIntArray,setIntArray)
INST_IN(I,P_Unbox,indexIntOffAddr,readIntOffAddr,writeIntOffAddr,setIntOffAddr)
INST_IN(I8,A_Unbox,indexInt8Array,readInt8Array,writeInt8Array,setInt8Array)
INST_IN(I8,P_Unbox,indexInt8OffAddr,readInt8OffAddr,writeInt8OffAddr,setInt8OffAddr)
INST_IN(I16,A_Unbox,indexInt16Array,readInt16Array,writeInt16Array,setInt16Array)
INST_IN(I16,P_Unbox,indexInt16OffAddr,readInt16OffAddr,writeInt16OffAddr,setInt16OffAddr)
INST_IN(I32,A_Unbox,indexInt32Array,readInt32Array,writeInt32Array,setInt32Array)
INST_IN(I32,P_Unbox,indexInt32OffAddr,readInt32OffAddr,writeInt32OffAddr,setInt32OffAddr)
INST_IN(I64,A_Unbox,indexInt64Array,readInt64Array,writeInt64Array,setInt64Array)
INST_IN(I64,P_Unbox,indexInt64OffAddr,readInt64OffAddr,writeInt64OffAddr,setInt64OffAddr)
INST_IN(U,A_Unbox,indexWordArray,readWordArray,writeWordArray,setWordArray)
INST_IN(U,P_Unbox,indexWordOffAddr,readWordOffAddr,writeWordOffAddr,setWordOffAddr)
INST_IN(U8,A_Unbox,indexWord8Array,readWord8Array,writeWord8Array,setWord8Array)
INST_IN(U8,P_Unbox,indexWord8OffAddr,readWord8OffAddr,writeWord8OffAddr,setWord8OffAddr)
INST_IN(U16,A_Unbox,indexWord16Array,readWord16Array,writeWord16Array,setWord16Array)
INST_IN(U16,P_Unbox,indexWord16OffAddr,readWord16OffAddr,writeWord16OffAddr,setWord16OffAddr)
INST_IN(U32,A_Unbox,indexWord32Array,readWord32Array,writeWord32Array,setWord32Array)
INST_IN(U32,P_Unbox,indexWord32OffAddr,readWord32OffAddr,writeWord32OffAddr,setWord32OffAddr)
INST_IN(U64,A_Unbox,indexWord64Array,readWord64Array,writeWord64Array,setWord64Array)
INST_IN(U64,P_Unbox,indexWord64OffAddr,readWord64OffAddr,writeWord64OffAddr,setWord64OffAddr)
INST_IN_SPEC(Char8,A_Unbox,indexCharArray,readCharArray,writeCharArray,setCharArray)
INST_IN_SPEC(Char8,P_Unbox,indexCharOffAddr,readCharOffAddr,writeCharOffAddr,setCharOffAddr)
INST_IN_SPEC(Char,A_Unbox,indexWideCharArray,readWideCharArray,writeWideCharArray,setWideCharArray)
INST_IN_SPEC(Char,P_Unbox,indexWideCharOffAddr,readWideCharOffAddr,writeWideCharOffAddr,setWideCharOffAddr)
INST_IN(Addr#,A_Unbox,indexAddrArray,readAddrArray,writeAddrArray,setAddrArray)
INST_IN(Addr#,P_Unbox,indexAddrOffAddr,readAddrOffAddr,writeAddrOffAddr,setAddrOffAddr)




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

-- | Bit indexing (no @read#@ or @write#@)
--instance B ∈ U where index# u i = B# (geWord# (and# u (uncheckedShiftL# 1## (word2Int# u))) 1## )

unio :: GHC.IO () -> ST_ s
unio (GHC.IO io) s = case unsafeCoerce# io s of (# s' , _ #) -> s'
{-# INLINE unio #-}
