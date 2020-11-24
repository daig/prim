-- | Description : Operations on arbitrary lifted types
module Any (module Any, Any, seq) where
import P
import qualified Array.Byte as Byte
import qualified Array.Boxed as Boxed
import GHC.Types as X (Any)

eq# ∷ a → a → B#
eq# = reallyUnsafePtrEquality#

fromP ∷ P → (# a #)
fromP = addrToAny#

-- | Must be run on an evaluated value, not a thunk
toP# ∷ a → IO# P
toP# = anyToAddr#

unpackClosure ∷ a → (# P, Byte.A, Boxed.A b #)
unpackClosure = unpackClosure# 

getApStackVal ∷ a → I → (# I, b #)
getApStackVal = getApStackVal#

size# ∷ a → I {- ^ # machine words -}
size# = closureSize#
