module F64 where
import HsFFI hiding (Inf,Inf_)
import GHC.Types (Double(D#),Bool(True))
import GHC.Classes (Eq(..))

decode2I ∷ F64 → (# I, U, U, I #) -- ^ (sign {1,-1}, high, low, exp)
decode2I = coerce decodeDouble_2Int#
decodeI64 ∷ F64 → (# I64, I #) -- ^ (mantissa , base-2 exponent)
decodeI64 = coerce decodeDouble_Int64#
