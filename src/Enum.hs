{-# language ScopedTypeVariables, RankNTypes, TypeApplications #-}
module Enum
  (-- | Must be evaluated, not a thunk
   dataToTag# 
  -- | @a@ must be an enum type
  ,tagToEnum#
  ) where

-- Note we can't fiddle with tagToEnum# eg to rename
-- because of ghc magic prefenting it used at higher order
