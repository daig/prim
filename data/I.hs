--------------------------------------------------------------------
-- | Description : Word-sized Integers
--------------------------------------------------------------------
module I (module I,module X) where
import GHC.Prim
import Cast as X
import Num as X

type I = Int#

pattern Max, Min ∷ I
pattern Max =  0x7FFFFFFFFFFFFFFF#
pattern Min = -0x8000000000000000#
