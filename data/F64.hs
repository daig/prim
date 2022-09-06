module F64 where
import HsFFI hiding (Inf,Inf_)
import GHC.Types (Double(D#),Bool(True))
import GHC.Classes (Eq(..))

decode2I ∷ F64 → (# I, U, U, I #) -- ^ (sign {1,-1}, high, low, exp)
decode2I = coerce decodeDouble_2Int#
decodeI64 ∷ F64 → (# I64, I #) -- ^ (mantissa , base-2 exponent)
decodeI64 = coerce decodeDouble_Int64#

{-
pattern Inf ∷ F64
pattern Inf ← (((1.0## / 0.0##) ≡) → T) where Inf = 1.0## / 0.0##
pattern Inf_ ∷ F64
pattern Inf_ ← (((-1.0## / 0.0##) ≡) → T) where Inf_ = -1.0## / 0.0##

pattern Max ∷ F64
pattern Max ← f64_max where Max = case f64_max of D# x → x
pattern Min ∷ F64
pattern Min ← f64_min where Min = case f64_min of D# x → x
pattern Eps ← ((\x → f64_epsilon == D# x) → True) where Eps = case f64_epsilon of D# x → x
-}
