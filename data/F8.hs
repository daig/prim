module F8 where
import HsFFI hiding (Inf,Inf_)
import GHC.Types (Double(D#),Bool(True))
import GHC.Classes (Eq(..))

decode2I ∷ F8 → (# I, U4, U4, I #) -- ^ (sign {1,-1}, high, low, exp)
decode2I x = case decodeDouble_2Int# x of
  (# s, cast → hi, cast → lo, e #) → (# s, hi, lo, e #)
decodeI8 ∷ F8 → (# I8, I #) -- ^ (mantissa , base-2 exponent)
decodeI8 = decodeDouble_Int64#
