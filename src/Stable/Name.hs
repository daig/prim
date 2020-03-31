module Stable.Name where

type Name = StableName#

new :: a -> IO (Name a)
new = makeStableName#
eq :: Name a -> Name a -> B
eq = eqStableName#
toInt :: Name a -> I64
toInt = stableNameToInt#
