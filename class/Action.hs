{-# language CPP #-}
module Action where
import GHC.CString
import Num
import Prim
import Cast
import Cmp
#include "MachDeps.h"

type (+.) ∷ ∀ {rp} {rx}. T rp → T rx → Constraint
class p +. x where (+.) ∷ p → x → p

type (+?) ∷ ∀ {rp} {rx}. T rp → T rx → Constraint
class p +. x ⇒ p +? x where (+?) ∷ p → x → (# (##) | p #)


type (-.) ∷ ∀ {ra} {rx}. T ra → T rx → Constraint
class p +. x ⇒ p -. x where (-.) ∷ p → p → x

-- |Advances the given address by the given offset (in bytes).
instance Addr# +. I where (+.) = plusAddr#
-- |Computes the offset (in bytes) required to get from the second to the first argument.
instance Addr# -. I where (-.) = coerce minusAddr#

#define INST_OFF_BOX_REF(A,CON)\
instance (A x       ) +. I where {CON# (# a, i #) +. j = CON# (# a, i + j #)} ;\
instance (A (x ∷ T_)) +. I where {CON# (# a, i #) +. j = CON# (# a, i + j #)}

INST_OFF_BOX_REF(ConstRef,Array_Off)
INST_OFF_BOX_REF(SmallConstRef,SmallArray_Off)
INST_OFF_BOX_REF(Ref s,MutableArray_Off)
INST_OFF_BOX_REF(SmallRef s,SmallMutableArray_Off)

#define INST_OFF_BOX_SLICE(A,CON)\
instance (A x       ) +. I where { CON# (# a, i, n #) +. j = CON# (# a, i + j, n - j #) } ;\
instance (A (x ∷ T_)) +. I where { CON# (# a, i, n #) +. j = CON# (# a, i + j, n - j #) } ;\
instance (A x       ) +? I where {x@(CON# (# a, i, n #)) +? j = if cast (j ≤ n) then (# | x +. j #) else  (# (##) | #) } ;\
instance (A (x ∷ T_)) +? I where {x@(CON# (# a, i, n #)) +? j = if cast (j ≤ n) then (# | x +. j #) else  (# (##) | #) } ;\

INST_OFF_BOX_SLICE(Slice,Array_Off_Len)
INST_OFF_BOX_SLICE(SmallSlice,SmallArray_Off_Len)
INST_OFF_BOX_SLICE(MutableSlice s,MutableArray_Off_Len)
INST_OFF_BOX_SLICE(SmallMutableSlice s,SmallMutableArray_Off_Len)

#define INST_OFF(TY) \
instance (ForeignArray# TY) +. I where {p +. i = coerce (`plusAddr#` (size @TY i)) p} ;\
instance (ForeignMutableArray# s TY) +. I where {p +. i = coerce (`plusAddr#` (size @TY i)) p} ;\
instance (ForeignArray# TY) -. I where {p -. q = coerce minusAddr# p q / size @TY 1#} ;\
instance (ForeignMutableArray# s TY) -. I where {p -. q = coerce minusAddr# p q / size @TY 1#} ;\
instance (UnboxedConstRef TY) +. I where {Bytes_Off# (# a, i #) +. j = Bytes_Off# (# a, i + j #)} ;\
instance (UnboxedRef s TY) +. I where {MBytes_Off# (# a, i #) +. j = MBytes_Off# (# a, i + j #)} ;\
instance (UnboxedSlice TY) +. I where { ;\
  Bytes_Off_Len# (# a, i, n #) +. j = Bytes_Off_Len# (# a, i + j, n - j #) } ;\
instance (UnboxedMutableSlice s TY) +. I where { ;\
  MBytes_Off_Len# (# a, i, n #) +. j = MBytes_Off_Len# (# a, i + j, n - j #) } ;\
instance (UnboxedSlice TY) +? I where { ;\
  x@(Bytes_Off_Len# (# a, i, n #)) +? j = if cast (j ≤ n) then (# | x +. j #) else  (# (##) | #) } ;\
instance (UnboxedMutableSlice s TY) +? I where { ;\
  x@(MBytes_Off_Len# (# a, i, n #)) +? j = if cast (j ≤ n) then (# | x +. j #) else  (# (##) | #) } ;\
instance (ForeignSlice TY) +. I where { ;\
  Addr_Len# (# a, n #) +. i = let j = size @TY i in Addr_Len# (# a +. j, n - j #) } ;\
instance (ForeignMutableSlice s TY) +. I where { ;\
  MAddr_Len# (# a, n #) +. i = let j = size @TY i in MAddr_Len# (# a +. j, n - j #) } ;\
instance (ForeignSlice TY) +? I where { ;\
  Addr_Len# x@(# a, n #) +? i = let j = size @TY i ;\
                                in if cast (j ≤ n) ;\
                                   then (# | Addr_Len# (# a +. j, n - j #) #) ;\
                                   else (# (##) | #) } ;\
instance (ForeignMutableSlice s TY) +? I where { ;\
  MAddr_Len# x@(# a, n #) +? i = let j = size @TY i ;\
                                 in if cast (j ≤ n) ;\
                                    then (# | MAddr_Len# (# a +. j, n - j #) #) ;\
                                    else (# (##) | #) }

INST_OFF(I)
INST_OFF(I8)
INST_OFF(I16)
INST_OFF(I32)
INST_OFF(I64)
INST_OFF(U)
INST_OFF(U8)
INST_OFF(U16)
INST_OFF(U32)
INST_OFF(U64)
INST_OFF(Addr#)
INST_OFF(Char#)
INST_OFF(Char8#)

type (.+) ∷ ∀ {rp} {rx}. T rp → T rx → Constraint
class x .+ p | x → p where (.+) ∷ x → p → p
instance S# Latin1 .+ [Char] where (.+) = coerce unpackAppendCString#
instance S# UTF8 .+ [Char] where (.+) = coerce unpackAppendCStringUtf8#

