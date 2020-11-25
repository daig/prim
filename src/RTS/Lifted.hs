--------------------------------------------------------------------
-- | Description : Operations on any lifted value
--------------------------------------------------------------------
module RTS.Any (seq, module RTS.Lifted) where

eq# ∷ a → a → B
eq# x y = coerce do reallyUnsafePtrEquality# x y

-- | Must be run on an evaluated value, not a thunk
toP# ∷ a → IO# P
toP# = anyToAddr#

fromP ∷ P → (# a #)
fromP = addrToAny#

-- | @unpackClosure\# closure@ copies the closure and pointers in the
--      payload of the given closure into two new arrays, and returns a pointer to
--      the first word of the closure\'s info table, a non-pointer array for the raw
--      bytes of the closure, and a pointer array for the pointers in the payload. 
--
-- see <https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects The Layout of Heap Objects>
unpackClosure ∷ a → (# P, A, Boxed.A b #)
unpackClosure = unpackClosure# 

size# ∷ a → I {- ^ # machine words -}
size# = closureSize#
