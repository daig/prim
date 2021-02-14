module F64 where

type F64# = Double#
{-
module Prim.F64 (F64 ,module Prim.F64) where
import HsFFI hiding (Inf,Inf_)
import Num as X


decode2I ∷ F64 → (# I8, U32, U32, I16 #) -- ^ (sign {1,-1}, high, low, exp)
decode2I = coerce decodeDouble_2Int#
decodeI64 ∷ F64 → (# I64, I16 #) -- ^ (mantissa , base-2 exponent)
decodeI64 = coerce decodeDouble_Int64#


pattern Max ∷ F64
pattern Max ← f64_max where Max = case f64_max of D# x → x
pattern Min ∷ F64
pattern Min ← f64_min where Min = case f64_min of D# x → x
pattern Eps ← ((\x → f64_epsilon == D# x) → True) where Eps = case f64_epsilon of D# x → x

-}
