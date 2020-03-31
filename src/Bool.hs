module Bool  (module Bool, module X)  where
import GHC.Types as X (isTrue#)

pattern O = 0#
pattern I = 1#
