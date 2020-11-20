-- | Description : Operations on arbitrary lifted types
module Any (module Any, Any, seq) where
import qualified Ref.Byte as Byte
import qualified Array
import GHC.Types as X (Any)

eq# ∷ a → a → B#
eq# = reallyUnsafePtrEquality#

fromRef ∷ Byte.Ref → (# a #)
fromRef = addrToAny#

-- | Must be run on an evaluated value, not a thunk
toRef# ∷ a → IO# Byte.Ref
toRef# = anyToAddr#

unpackClosure ∷ a → (# Byte.Ref, Array.Byte, Array.Boxed b #)
unpackClosure = unpackClosure# 

getApStackVal ∷ a → I → (# I, b #)
getApStackVal = getApStackVal#

size# ∷ a → I {- ^ # machine words -}
size# = closureSize#
