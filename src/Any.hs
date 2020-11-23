-- | Description : Operations on arbitrary lifted types
module Any (module Any, Any, seq) where
import qualified P.Byte as Byte
import qualified Array.Byte as Byte
import qualified Array.Boxed as Boxed
import GHC.Types as X (Any)

eq# ∷ a → a → B#
eq# = reallyUnsafePtrEquality#

fromP ∷ Byte.P → (# a #)
fromP = addrToAny#

-- | Must be run on an evaluated value, not a thunk
toP# ∷ a → IO# Byte.P
toP# = anyToAddr#

unpackClosure ∷ a → (# Byte.P, Byte.A, Boxed.A b #)
unpackClosure = unpackClosure# 

getApStackVal ∷ a → I → (# I, b #)
getApStackVal = getApStackVal#

size# ∷ a → I {- ^ # machine words -}
size# = closureSize#
