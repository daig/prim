{-# language CPP #-}
module Var where
import Cast
import Array.Index

type Write ∷ ∀ {ra} {r}. T ra → (★ → T ra → T r) → C
class Write x p where (.=) ∷ p s x → x → ST_ s
type Read ∷ ∀ {ra} {r}. T ra → (★ → T ra → T r) → C
class Read x p where read ∷ p s x → ST s x

instance Write x MutVar# where (.=) = writeMutVar#
instance Read x MutVar# where read = readMutVar#

instance Read x TVar# where read = readTVar#
instance Write x TVar# where (.=) = writeTVar#

#define INST_VAR(TY,GET,READ,READ_REF,WRITE,WRITE_REF) \
instance (x ≑ TY) ⇒ Write (x) ForeignMutableArray# where { (.=) = coerce (`WRITE#` 0#) } ;\
instance  Write (TY) UnboxedRef where { MBytes_Off# (# a, i #) .= x = WRITE_REF# a i x } ;\
instance  Read (TY) UnboxedRef where { read( MBytes_Off# (# a, i #)) = READ_REF# a i } ;\
instance (x ≑ TY) ⇒ Read (x) ForeignMutableArray# where { read = coerce (`READ#` 0#)}

-- | Set the entire slice
instance x ∈# ForeignArray# ⇒ Write x ForeignMutableSlice where (MAddr_Len# (# p, n #)) .= x = set# (Addr# p) 0# n x

-- | Set the entire slice
instance x ∈# UnboxedArray# ⇒ Write x UnboxedMutableSlice where (MBytes_Off_Len# (# a, i, n #)) .= x = set# (MutableByteArray# a) i n x

#define INST_VAR_SPEC(TY,GET,READ,READ_REF,WRITE,WRITE_REF) \
instance {-# OVERLAPPING #-} Write (TY) ForeignMutableArray# where { (.=) = coerce (`WRITE#` 0#) } ;\
instance  Write (TY) UnboxedRef where { MBytes_Off# (# a, i #) .= x = coerce WRITE_REF# a i x} ;\
instance  Read (TY) UnboxedRef where { read (MBytes_Off# (# a, i #)) = coerce READ_REF# a i} ;\
instance {-# OVERLAPPING #-} Read (TY) ForeignMutableArray# where { read = coerce (`READ#` 0#) }

INST_VAR(I,indexIntOffAddr,readIntOffAddr,readIntArray,writeIntOffAddr,writeIntArray)
INST_VAR(I8,indexInt8OffAddr,readInt8OffAddr,readInt8Array,writeInt8OffAddr,writeInt8Array)
INST_VAR(I16,indexInt16OffAddr,readInt16OffAddr,readInt16Array,writeInt16OffAddr,writeInt16Array)
INST_VAR(I32,indexInt32OffAddr,readInt32OffAddr,readInt32Array,writeInt32OffAddr,writeInt32Array)
INST_VAR(I64,indexInt64OffAddr,readInt64OffAddr,readInt64Array,writeInt64OffAddr,writeInt64Array)
INST_VAR(U,indexWordOffAddr,readWordOffAddr,readWordArray,writeWordOffAddr,writeWordArray)
INST_VAR(U8,indexWord8OffAddr,readWord8OffAddr,readWord8Array,writeWord8OffAddr,writeWord8Array)
INST_VAR(U16,indexWord16OffAddr,readWord16OffAddr,readWord16Array,writeWord16OffAddr,writeWord16Array)
INST_VAR(U32,indexWord32OffAddr,readWord32OffAddr,readWord32Array,writeWord32OffAddr,writeWord32Array)
INST_VAR(U64,indexWord64OffAddr,readWord64OffAddr,readWord64Array,writeWord64OffAddr,writeWord64Array)
INST_VAR(Addr#,indexAddrOffAddr,readAddrOffAddr,readAddrArray,writeAddrOffAddr,writeAddrArray)
INST_VAR_SPEC(Char8#,indexCharOffAddr,readCharOffAddr,readCharArray,writeCharOffAddr,writeCharArray)
INST_VAR_SPEC(Char#,indexWideCharOffAddr,readWideCharOffAddr,readWideCharArray,writeWideCharOffAddr,writeWideCharArray)
