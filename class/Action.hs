{-# language CPP #-}
module Action where
import GHC.CString
import Num
import Prim
#include "MachDeps.h"

type (+.) ∷ ∀ {rp} {rx}. T rp → T rx → Constraint
class p +. x where (+.) ∷ p → x → p

type (-.) ∷ ∀ {ra} {rx}. T ra → T rx → Constraint
class p +. x ⇒ p -. x where (-.) ∷ p → p → x

-- |Advances the given address by the given offset (in bytes).
instance Addr# +. I where (+.) = plusAddr#
-- |Computes the offset (in bytes) required to get from the second to the first argument.
instance Addr# -. I where (-.) = coerce minusAddr#

#define INST_OFF(TYPE) \
instance (ForeignArray# TYPE) +. I where {p +. i = coerce (`plusAddr#` (size @TYPE i)) p} ;\
instance (ForeignMutableArray# s TYPE) +. I where {p +. i = coerce (`plusAddr#` (size @TYPE i)) p} ;\
instance (ForeignArray# TYPE) -. I where {p -. q = coerce minusAddr# p q / size @TYPE 1#} ;\
instance (ForeignMutableArray# s TYPE) -. I where {p -. q = coerce minusAddr# p q / size @TYPE 1#} ;\

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

