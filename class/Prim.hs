{-# language CPP #-}
module Prim where
import {-# source #-} Num

#include "MachDeps.h"
#include "HsBaseConfig.h"

type Elt ∷ ∀ {rx} {r}. (T rx → T r) → T rx → TC
type family Elt a where
  Elt Array# = OK
  Elt (MutableArray# s) = OK
  Elt SmallArray# = OK
  Elt (SmallMutableArray# s) = OK
  Elt A' = Prim
  Elt A'# = Prim
  Elt A'## = Prim
  Elt A'_ = Prim
  Elt A'_# = Prim
  Elt A'_## = Prim
  Elt (A_ s) = Prim
  Elt (A_# s) = Prim
  Elt (A_## s) = Prim
  Elt (A s) = Prim
  Elt (A# s) = Prim
  Elt (A## s) = Prim
  Elt P' = Prim
  Elt P'## = Prim
  Elt (P s) = Prim
  Elt (P## s) = Prim
  Elt (MutVar# s) = OK
  Elt (TVar# s) = OK
  Elt (MVar# s) = OK
  Elt (IOPort# s) = OK
  
-- | Primitive unboxed, unlifted types that fit natively into raw memory
type Prim ∷ forall {r}. T r → TC
class Prim (x ∷ T r) where
  size ∷ I {- ^ # elements -} → I {- ^ size in bytes -}
  align ∷ I → I
  (!#) ∷ A' U1 → I {- ^ index in bytes -} → x
  (!!#) ∷ A s U1 → I {- ^ index in # bytes -} → ST s x
  write# ∷ A s U1 → I {- ^ index in # bytes -} → x → ST_ s

#define INST_PRIM(TY,SI,AL,IB,RB,WB) \
instance (Coercible x TY) ⇒ Prim (x) where {\
  size = (SI# *); \
  align i = case i % AL# of {0# → i; off → i + (AL# - off)}; \
  (!#) x = coerce IB# x; \
  (!!#) = coerce RB# ; \
  write# = coerce WB# }
#define INST_PRIM_SPEC(TY,SI,AL,IB,RB,WB) \
instance {-# OVERLAPPING #-} Prim (TY) where {\
  size = (SI# *); \
  align i = case i % AL# of {0# → i; off → i + (AL# - off)}; \
  (!#) x = coerce IB# x; \
  (!!#) = coerce RB# ; \
  write# = coerce WB# }

INST_PRIM(I,SIZEOF_HSINT,ALIGNMENT_HSINT,indexWord8ArrayAsInt,readWord8ArrayAsInt,writeWord8ArrayAsInt)
INST_PRIM(I1,SIZEOF_INT8,ALIGNMENT_INT8,indexInt8Array,readInt8Array,writeInt8Array)
INST_PRIM(I2,SIZEOF_INT16,ALIGNMENT_INT16,indexWord8ArrayAsInt16,readWord8ArrayAsInt16,writeWord8ArrayAsInt16)
INST_PRIM(I4,SIZEOF_INT32,ALIGNMENT_INT32,indexWord8ArrayAsInt32,readWord8ArrayAsInt32,writeWord8ArrayAsInt32)
INST_PRIM(I8,SIZEOF_INT64,ALIGNMENT_INT64,indexWord8ArrayAsInt64,readWord8ArrayAsInt64,writeWord8ArrayAsInt64)
INST_PRIM(U,SIZEOF_HSWORD,ALIGNMENT_HSWORD,indexWord8ArrayAsWord,readWord8ArrayAsWord,writeWord8ArrayAsWord)
INST_PRIM(U1,SIZEOF_WORD8,ALIGNMENT_WORD8,indexWord8Array,readWord8Array,writeWord8Array)
INST_PRIM(U2,SIZEOF_WORD16,ALIGNMENT_WORD16,indexWord8ArrayAsWord16,readWord8ArrayAsWord16,writeWord8ArrayAsWord16)
INST_PRIM(U4,SIZEOF_WORD32,ALIGNMENT_WORD32,indexWord8ArrayAsWord32,readWord8ArrayAsWord32,writeWord8ArrayAsWord32)
INST_PRIM(U8,SIZEOF_WORD64,ALIGNMENT_WORD64,indexWord8ArrayAsWord64,readWord8ArrayAsWord64,writeWord8ArrayAsWord64)
INST_PRIM(F4,SIZEOF_FLOAT,ALIGNMENT_FLOAT,indexWord8ArrayAsFloat,readWord8ArrayAsFloat,writeWord8ArrayAsFloat)
INST_PRIM(F8,SIZEOF_DOUBLE,ALIGNMENT_DOUBLE,indexWord8ArrayAsDouble,readWord8ArrayAsDouble,writeWord8ArrayAsDouble)
INST_PRIM(Addr#,SIZEOF_HSPTR,ALIGNMENT_HSPTR,indexWord8ArrayAsAddr,readWord8ArrayAsAddr,writeWord8ArrayAsAddr)

-- Override the word representation for chars
INST_PRIM_SPEC(C,SIZEOF_HSCHAR,ALIGNMENT_HSCHAR,indexWord8ArrayAsWideChar,readWord8ArrayAsWideChar,writeWord8ArrayAsWideChar)
INST_PRIM_SPEC(C1,1,ALIGNMENT_HSCHAR,indexWord8ArrayAsChar,readWord8ArrayAsChar,writeWord8ArrayAsChar)
