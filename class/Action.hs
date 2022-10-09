{-# language CPP #-}
--------------------------------------------------------------------
-- | Description : Operations to advance memory indexes
--------------------------------------------------------------------
module Action where
import GHC.CString
import Num
import Prim
import Cast
import Cmp
#include "MachDeps.h"

type (+.) ∷ ∀ {rp} {rx}. T rp → T rx → TC
class p +. x where (+.) ∷ p → x → p

type (+?) ∷ ∀ {rp} {rx}. T rp → T rx → TC
class p +. x ⇒ p +? x where (+?) ∷ p → x → (# (##) | p #)


type (-.) ∷ ∀ {ra} {rx}. T ra → T rx → TC
class p +. x ⇒ p -. x where (-.) ∷ p → p → x

-- |Advances the given address by the given offset (in bytes).
instance Addr# +. I where (+.) = plusAddr#
-- |Computes the offset (in bytes) required to get from the second to the first argument.
instance Addr# -. I where (-.) = coerce minusAddr#

instance (AR'# x       ) +. I where {AR'_Off# (# a, i #) +. j = AR'_Off# (# a, i + j #)} ;\
instance (AR'# (x ∷ T_)) +. I where {AR'_Off# (# a, i #) +. j = AR'_Off# (# a, i + j #)}

instance (Ar'# x       ) +. I where {Ar'_Off# (# a, i #) +. j = Ar'_Off# (# a, i + j #)} ;\
instance (Ar'# (x ∷ T_)) +. I where {Ar'_Off# (# a, i #) +. j = Ar'_Off# (# a, i + j #)}

instance (AR# s x       ) +. I where {AR_Off# (# a, i #) +. j = AR_Off# (# a, i + j #)} ;\
instance (AR# s (x ∷ T_)) +. I where {AR_Off# (# a, i #) +. j = AR_Off# (# a, i + j #)}

instance (Ar# s x       ) +. I where {Ar_Off# (# a, i #) +. j = Ar_Off# (# a, i + j #)} ;\
instance (Ar# s (x ∷ T_)) +. I where {Ar_Off# (# a, i #) +. j = Ar_Off# (# a, i + j #)}

instance (AR'## x       ) +. I where { AR'_Off_Len# (# a, i, n #) +. j = AR'_Off_Len# (# a, i + j, n - j #) } ;\
instance (AR'## (x ∷ T_)) +. I where { AR'_Off_Len# (# a, i, n #) +. j = AR'_Off_Len# (# a, i + j, n - j #) } ;\
instance (AR'## x       ) +? I where {x@(AR'_Off_Len# (# a, i, n #)) +? j = if j <= n then (# | x +. j #) else  (# (##) | #) } ;\
instance (AR'## (x ∷ T_)) +? I where {x@(AR'_Off_Len# (# a, i, n #)) +? j = if j <= n then (# | x +. j #) else  (# (##) | #) } ;\

instance (Ar'## x       ) +. I where { Ar'_Off_Len# (# a, i, n #) +. j = Ar'_Off_Len# (# a, i + j, n - j #) } ;\
instance (Ar'## (x ∷ T_)) +. I where { Ar'_Off_Len# (# a, i, n #) +. j = Ar'_Off_Len# (# a, i + j, n - j #) } ;\
instance (Ar'## x       ) +? I where {x@(Ar'_Off_Len# (# a, i, n #)) +? j = if j <= n then (# | x +. j #) else  (# (##) | #) } ;\
instance (Ar'## (x ∷ T_)) +? I where {x@(Ar'_Off_Len# (# a, i, n #)) +? j = if j <= n then (# | x +. j #) else  (# (##) | #) } ;\

instance (AR## s x       ) +. I where { AR_Off_Len# (# a, i, n #) +. j = AR_Off_Len# (# a, i + j, n - j #) } ;\
instance (AR## s (x ∷ T_)) +. I where { AR_Off_Len# (# a, i, n #) +. j = AR_Off_Len# (# a, i + j, n - j #) } ;\
instance (AR## s x       ) +? I where {x@(AR_Off_Len# (# a, i, n #)) +? j = if j <= n then (# | x +. j #) else  (# (##) | #) } ;\
instance (AR## s (x ∷ T_)) +? I where {x@(AR_Off_Len# (# a, i, n #)) +? j = if j <= n then (# | x +. j #) else  (# (##) | #) } ;\

instance (Ar## s x       ) +. I where { Ar_Off_Len# (# a, i, n #) +. j = Ar_Off_Len# (# a, i + j, n - j #) } ;\
instance (Ar## s (x ∷ T_)) +. I where { Ar_Off_Len# (# a, i, n #) +. j = Ar_Off_Len# (# a, i + j, n - j #) } ;\
instance (Ar## s x       ) +? I where {x@(Ar_Off_Len# (# a, i, n #)) +? j = if j <= n then (# | x +. j #) else  (# (##) | #) } ;\
instance (Ar## s (x ∷ T_)) +? I where {x@(Ar_Off_Len# (# a, i, n #)) +? j = if j <= n then (# | x +. j #) else  (# (##) | #) } ;\

#define INST_OFF(TY) \
instance (P' TY) +. I where {p +. i = coerce (`plusAddr#` (size @TY i)) p} ;\
instance (P s TY) +. I where {p +. i = coerce (`plusAddr#` (size @TY i)) p} ;\
instance (P' TY) -. I where {p -. q = coerce minusAddr# p q / size @TY 1#} ;\
instance (P s TY) -. I where {p -. q = coerce minusAddr# p q / size @TY 1#} ;\
instance (A'# TY) +. I where {Bytes'_Off# (# a, i #) +. j = Bytes'_Off# (# a, i + j #)} ;\
instance (A# s TY) +. I where {Bytes_Off# (# a, i #) +. j = Bytes_Off# (# a, i + j #)} ;\
instance (A'## TY) +. I where { ;\
  Bytes'_Off_Len# (# a, i, n #) +. j = Bytes'_Off_Len# (# a, i + j, n - j #) } ;\
instance (A## s TY) +. I where { ;\
  Bytes_Off_Len# (# a, i, n #) +. j = Bytes_Off_Len# (# a, i + j, n - j #) } ;\
instance (A'## TY) +? I where { ;\
  x@(Bytes'_Off_Len# (# a, i, n #)) +? j = if j <= n then (# | x +. j #) else  (# (##) | #) } ;\
instance (A## s TY) +? I where { ;\
  x@(Bytes_Off_Len# (# a, i, n #)) +? j = if j <= n then (# | x +. j #) else  (# (##) | #) } ;\
instance (P'## TY) +. I where { ;\
  P'_Len# (# a, n #) +. i = let j = size @TY i in P'_Len# (# a +. j, n - j #) } ;\
instance (P## s TY) +. I where { ;\
  P_Len# (# a, n #) +. i = let j = size @TY i in P_Len# (# a +. j, n - j #) } ;\
instance (P'## TY) +? I where { ;\
  P'_Len# x@(# a, n #) +? i = let j = size @TY i ;\
                                in if j <= n ;\
                                   then (# | P'_Len# (# a +. j, n - j #) #) ;\
                                   else (# (##) | #) } ;\
instance (P## s TY) +? I where { ;\
  P_Len# x@(# a, n #) +? i = let j = size @TY i ;\
                                 in if j <= n ;\
                                    then (# | P_Len# (# a +. j, n - j #) #) ;\
                                    else (# (##) | #) }

INST_OFF(I)
INST_OFF(I1)
INST_OFF(I2)
INST_OFF(I4)
INST_OFF(I8)
INST_OFF(U)
INST_OFF(U1)
INST_OFF(U2)
INST_OFF(U4)
INST_OFF(U8)
INST_OFF(Addr#)
INST_OFF(C)
INST_OFF(C1)


