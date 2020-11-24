-- | Description : Operations on arbitrary lifted types
module Any (module Any, Any, seq) where
import P
import A
import qualified A.Boxed as Boxed
import GHC.Types as X (Any)

eq# ∷ a → a → B#
eq# = reallyUnsafePtrEquality#

fromP ∷ P → (# a #)
fromP = addrToAny#

-- | Must be run on an evaluated value, not a thunk
toP# ∷ a → IO# P
toP# = anyToAddr#

unpackClosure ∷ a → (# P, A, Boxed.A b #)
unpackClosure = unpackClosure# 

getApStackVal ∷ a → I → (# I, b #)
getApStackVal = getApStackVal#

size# ∷ a → I {- ^ # machine words -}
size# = closureSize#

-- | Keep a value alive to the GC.
-- It only makes sense to apply touch to lifted types on the heap.
--
-- see <https://gitlab.haskell.org/ghc/ghc/-/wikis/hidden-dangers-of-touch The Hidden Dangers of touch#>
touch ∷ k → IO_#
touch = touch#
