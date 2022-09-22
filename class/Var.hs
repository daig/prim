{-# language QuantifiedConstraints, CPP #-}
module Var where
import Cast

type Var ∷ ∀ {ra} {r}. T ra → (★ → T ra → T r) → C
class Var x p where
  (.=) ∷ p s x → x → ST_ s
  read ∷ p s x → ST s x

instance Var x MutVar# where
  read = readMutVar#
  (.=) = writeMutVar#

instance Var x TVar# where
  read = readTVar#
  (.=) = writeTVar#

#define INST_VAR(TY,GET,READ,WRITE) \
instance (x ≑ TY) ⇒ Var (x) ForeignMutableArray# where { \
  read = coerce (`READ#` 0#) ;\
  (.=) = coerce (`WRITE#` 0#) }

#define INST_VAR_SPEC(TY,GET,READ,WRITE) \
instance {-# OVERLAPPING #-} Var (TY) ForeignMutableArray# where { \
  read = coerce (`READ#` 0#) ;\
  (.=) = coerce (`WRITE#` 0#) }

INST_VAR(I,indexIntOffAddr,readIntOffAddr,writeIntOffAddr)
INST_VAR(I8,indexInt8OffAddr,readInt8OffAddr,writeInt8OffAddr)
INST_VAR(I16,indexInt16OffAddr,readInt16OffAddr,writeInt16OffAddr)
INST_VAR(I32,indexInt32OffAddr,readInt32OffAddr,writeInt32OffAddr)
INST_VAR(I64,indexInt64OffAddr,readInt64OffAddr,writeInt64OffAddr)
INST_VAR(U,indexWordOffAddr,readWordOffAddr,writeWordOffAddr)
INST_VAR(U8,indexWord8OffAddr,readWord8OffAddr,writeWord8OffAddr)
INST_VAR(U16,indexWord16OffAddr,readWord16OffAddr,writeWord16OffAddr)
INST_VAR(U32,indexWord32OffAddr,readWord32OffAddr,writeWord32OffAddr)
INST_VAR(U64,indexWord64OffAddr,readWord64OffAddr,writeWord64OffAddr)
INST_VAR(Addr#,indexAddrOffAddr,readAddrOffAddr,writeAddrOffAddr)
INST_VAR_SPEC(Char8#,indexCharOffAddr,readCharOffAddr,writeCharOffAddr)
INST_VAR_SPEC(Char#,indexWideCharOffAddr,readWideCharOffAddr,writeWideCharOffAddr)
