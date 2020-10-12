{-# language BangPatterns, ScopedTypeVariables, RankNTypes, TypeApplications #-}
module Enum
  (-- | Must be evaluated, not a thunk
   dataToTag# 
  -- | @a@ must be an enum type
  ,tagToEnum#
  ,toI
  ) where

-- Note we can't fiddle with tagToEnum# eg to rename
-- because of ghc magic prefenting it used at higher order


{- |
Returns the 'tag' of a constructor application; this function is used
by the deriving code for Eq, Ord and Enum.

The primitive dataToTag# requires an evaluated constructor application
as its argument, so we provide getTag as a wrapper that performs the
evaluation before calling dataToTag#.  We could have dataToTag#
evaluate its argument, but we prefer to do it this way because (a)
dataToTag# can be an inline primop if it doesn't need to do any
evaluation, and (b) we want to expose the evaluation to the
simplifier, because it might be possible to eliminate the evaluation
in the case when the argument is already known to be evaluated. -}
toI ∷ a → I; {-# inline toI #-}
toI !x = dataToTag# x
