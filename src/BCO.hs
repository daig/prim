module BCO where

mkApUpd0 :: BCO -> (# a #)
mkApUpd0 = mkApUpd0#

new :: Bytes -> Bytes -> Array a -> I64 -> Bytes -> ST s BCO
new = newBCO#
