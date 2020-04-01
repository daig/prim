module BCO where
import qualified Array

mkApUpd0 :: BCO -> (# a #)
mkApUpd0 = mkApUpd0#

new :: Array.Byte -> Array.Byte -> Array.Boxed a -> I64 -> Array.Byte -> ST s BCO
new = newBCO#
