--------------------------------------------------------------------
-- | Description : Builtin boxed types and classes 
--
-- These types get magically wrapped by the FFI
-- These classes are magically stock derivable.
--------------------------------------------------------------------
module Stock (module X) where
import Stock.Char as X
import Stock.Double as X
import Stock.Float as X
import Stock.Eq as X
import Stock.Ord as X
import Stock.IO as X
import Stock.Int as X
import Stock.Word as X
