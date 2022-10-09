--------------------------------------------------------------------
-- | Description : Operations on any lifted value
--------------------------------------------------------------------
{-# language BangPatterns #-}
module Any
  (seq
  -- | Must be evaluated, not a thunk
  ,dataToTag# 
  -- | @a@ must be an enum type (no payload)
  ,tagToEnum#
  ,toI
  ,eq#,eq,toP#,fromP#
  ) where
import Coerce

-- Note we can't fiddle with tagToEnum# eg to rename
-- because of ghc magic preventing it used at higher order

-- | Compare pointers, which may be moved by the GC.
-- __/Warning:/__ this can fail with an unchecked exception.
eq# ∷ a → a → B#
eq# x y = coerce do reallyUnsafePtrEquality# x y

-- | Compare pointers, which may be moved by the GC.
-- __/Warning:/__ this can fail with an unchecked exception.
eq ∷ ∀ a. a → a → Bool
eq = cast (eq# @a)

-- | Find the address of an evaluated value (not a thunk)
toP# ∷ a → IO Addr#
toP# = anyToAddr#

-- | Interpret value if valid or fail spectacularly.
-- The addressing happens when the unboxed tuple is matched,
-- but the value is not evaluated.
fromP# ∷ Addr# → (# a #)
fromP# = addrToAny#

-- | Copies the closure and pointers in the
--      payload of the given closure into two new arrays, and returns a pointer to
--      the first word of the closure\'s info table, a non-pointer array for the raw
--      bytes of the closure, and a pointer array for the pointers in the payload. 
--
-- see <https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects The Layout of Heap Objects>
unpack ∷ ∀ a b. a → (# P# , A' U8 , AR' b #)
unpack = coerce (unpackClosure# @a @b)

size# ∷ a → I {- ^ # machine words -}
size# = closureSize#

{- |
Returns the tag of a constructor application; this function is used
by the deriving code for Eq, Ord and Enum.

The primitive dataToTag# requires an evaluated constructor application
as its argument, so we provide @toI@ as a wrapper that performs the
evaluation before calling dataToTag#.  We could have dataToTag#
evaluate its argument, but we prefer to do it this way because (a)
dataToTag# can be an inline primop if it doesn't need to do any
evaluation, and (b) we want to expose the evaluation to the
simplifier, because it might be possible to eliminate the evaluation
in the case when the argument is already known to be evaluated. -}
toI ∷ a → I; {-# inline toI #-}
toI !x = dataToTag# x

to ∷ a → Any; {-# inline to #-}
to = unsafeCoerce#

from ∷ Any → a; {-# inline from #-}
from = unsafeCoerce#
