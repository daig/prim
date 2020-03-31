{-# language ScopedTypeVariables, RankNTypes, TypeApplications #-}
module Enum
  (dataToTag# -- ^ Must be evaluated, not a thunk
  ,tagToEnum# -- ^ @a@ must be an enum type
  ) where

-- Note we can't fiddle with tagToEnum# eg to rename
-- because of ghc magic prefenting it used at higher order
