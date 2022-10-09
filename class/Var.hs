{-# language CPP #-}
module Var where
import Cast
import Array.Index
import Do.ST

type Write ∷ ∀ {ra} {r}. T ra → (★ → T ra → T r) → TC
class Write x p where (.=) ∷ p s x → x → ST_ s
type Read ∷ ∀ {ra} {r}. T ra → (★ → T ra → T r) → TC
class Read x p where read ∷ p s x → ST s x

instance Write x MutVar# where (.=) = writeMutVar#
instance Read x MutVar# where read = readMutVar#

instance Read x TVar# where read = readTVar#
instance Write x TVar# where (.=) = writeTVar#

instance Read x MVar# where read = takeMVar#
instance Write x MVar# where (.=) = putMVar#

instance Read x IOPort# where read = readIOPort#
instance Write x IOPort# where r .= x = \s → case writeIOPort# r x s of (# ss , _ #) → ss

#define INST_VAR(TY,GET,READ,READ_REF,WRITE,WRITE_REF) \
instance (Coercible x TY) ⇒ Write (x) P where { (.=) = coerce (`WRITE#` 0#) } ;\
instance  Write (TY) A# where { Bytes_Off# (# a, i #) .= x = WRITE_REF# a i x } ;\
instance  Read (TY) A# where { read( Bytes_Off# (# a, i #)) = READ_REF# a i } ;\
instance (Coercible x TY) ⇒ Read (x) P where { read = coerce (`READ#` 0#)}

-- | Set the entire slice
instance Index x P' ⇒ Write x P## where (P_Len# (# p, n #)) .= x = set# (P# p) 0# n x

-- | Set the entire slice
instance Index x A' ⇒ Write x A## where (Bytes_Off_Len# (# a, i, n #)) .= x = set# (A# a) i n x

#define INST_VAR_SPEC(TY,GET,READ,READ_REF,WRITE,WRITE_REF) \
instance {-# OVERLAPPING #-} Write (TY) P where { (.=) = coerce (`WRITE#` 0#) } ;\
instance  Write (TY) A# where { Bytes_Off# (# a, i #) .= x = coerce WRITE_REF# a i x} ;\
instance  Read (TY) A# where { read (Bytes_Off# (# a, i #)) = coerce READ_REF# a i} ;\
instance {-# OVERLAPPING #-} Read (TY) P where { read = coerce (`READ#` 0#) }

INST_VAR(I,indexIntOffAddr,readIntOffAddr,readIntArray,writeIntOffAddr,writeIntArray)
INST_VAR(I1,indexInt8OffAddr,readInt8OffAddr,readInt8Array,writeInt8OffAddr,writeInt8Array)
INST_VAR(I2,indexInt16OffAddr,readInt16OffAddr,readInt16Array,writeInt16OffAddr,writeInt16Array)
INST_VAR(I4,indexInt32OffAddr,readInt32OffAddr,readInt32Array,writeInt32OffAddr,writeInt32Array)
INST_VAR(I8,indexInt64OffAddr,readInt64OffAddr,readInt64Array,writeInt64OffAddr,writeInt64Array)
INST_VAR(U,indexWordOffAddr,readWordOffAddr,readWordArray,writeWordOffAddr,writeWordArray)
INST_VAR(U1,indexWord8OffAddr,readWord8OffAddr,readWord8Array,writeWord8OffAddr,writeWord8Array)
INST_VAR(U2,indexWord16OffAddr,readWord16OffAddr,readWord16Array,writeWord16OffAddr,writeWord16Array)
INST_VAR(U4,indexWord32OffAddr,readWord32OffAddr,readWord32Array,writeWord32OffAddr,writeWord32Array)
INST_VAR(U8,indexWord64OffAddr,readWord64OffAddr,readWord64Array,writeWord64OffAddr,writeWord64Array)
INST_VAR(Addr#,indexAddrOffAddr,readAddrOffAddr,readAddrArray,writeAddrOffAddr,writeAddrArray)
INST_VAR_SPEC(C1,indexCharOffAddr,readCharOffAddr,readCharArray,writeCharOffAddr,writeCharArray)
INST_VAR_SPEC(C,indexWideCharOffAddr,readWideCharOffAddr,readWideCharArray,writeWideCharOffAddr,writeWideCharArray)
