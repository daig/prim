module BCO where
import qualified Array.Byte as Byte
import Array (Array)

mkApUpd0 :: BCO -> (# a #)
mkApUpd0 = mkApUpd0#

new :: Byte.Array -> Byte.Array -> Array a -> I64 -> Byte.Array -> ST s BCO
new = newBCO#
