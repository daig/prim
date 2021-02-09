-------------------------------------------------------------------- -- | Description : eXport for builtin boxed types and classes
--
-- These types get magically wrapped by the FFI
-- These classes are magically stock derivable.
--------------------------------------------------------------------
module X (module X) where
import Char as X
import IO as X
import Int as X
import Double as X
import Float as X
import Eq as X
import Ord as X
import Word as X
