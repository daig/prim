{-# language CPP #-}
module Action where
import GHC.CString
import Num
#include "MachDeps.h"

type (+.) ∷ ∀ {rp} {rx}. T rp → T rx → Constraint
class p +. x where (+.) ∷ p → x → p

type (-.) ∷ ∀ {ra} {rx}. T ra → T rx → Constraint
class p +. x ⇒ p -. x where (-.) ∷ p → p → x

-- |Advances the given address by the given offset (in bytes).
instance Addr# +. I where (+.) = plusAddr#
-- |Computes the offset (in bytes) required to get from the second to the first argument.
instance Addr# -. I where (-.) = coerce minusAddr#

#define INST_OFF(TYPE,SIZE) \
instance (Const AddrVar# TYPE) +. I where {p +. i = coerce (`plusAddr#` (i *# SIZE#)) p} ;\
instance (AddrVar# s TYPE) +. I where {p +. i = coerce (`plusAddr#` (i *# SIZE#)) p} ;\
instance (AddrVar# s TYPE) -. I where {p -. q = coerce minusAddr# p q / SIZE#} ;\
instance (Const AddrVar# TYPE) -. I where {p -. q = coerce minusAddr# p q / SIZE#}

#define INST_OFF0(TYPE) \
instance (Const AddrVar# TYPE) +. I where {(+.) = coerce plusAddr#} ;\
instance (AddrVar# s TYPE) +. I where {(+.) = coerce plusAddr#} ;\
instance (Const AddrVar# TYPE) -. I where {(-.) = coerce minusAddr#} ;\
instance (AddrVar# s TYPE) -. I where {(-.) = coerce minusAddr#}

INST_OFF(I,SIZEOF_HSINT)
INST_OFF0(I8)
INST_OFF(I16,SIZEOF_INT16)
INST_OFF(I32,SIZEOF_INT32)
INST_OFF(I64,SIZEOF_INT64)
INST_OFF(U,SIZEOF_HSWORD)
INST_OFF0(U8)
INST_OFF(U16,SIZEOF_WORD16)
INST_OFF(U32,SIZEOF_WORD32)
INST_OFF(U64,SIZEOF_WORD64)
INST_OFF(Addr#,SIZEOF_HSPTR)
INST_OFF(Char,4)
INST_OFF0(Char8#)

type (.+) ∷ ∀ {rp} {rx}. T rp → T rx → Constraint
class x .+ p | x → p where (.+) ∷ x → p → p
instance S# Latin1 .+ [Char] where (.+) = coerce unpackAppendCString#
instance S# UTF8 .+ [Char] where (.+) = coerce unpackAppendCStringUtf8#

