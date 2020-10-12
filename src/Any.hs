module Any (module Any, Any, seq) where
import qualified Ref
import qualified Array
import GHC.Types as X (Any)

eq# ∷ a → a → I1
eq# = reallyUnsafePtrEquality#

fromRef ∷ Ref.Byte → (# a #)
fromRef = addrToAny#

-- | Must be run on an evaluated value, not a thunk
toRef# ∷ a → IO Ref.Byte
toRef# = anyToAddr#

unpackClosure ∷ a → (# Ref.Byte, Array.Byte, Array.Boxed b #)
unpackClosure = unpackClosure# 

getApStackVal ∷ a → I64 → (# I64, b #)
getApStackVal = getApStackVal#
