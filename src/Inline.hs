--------------------------------------------------------------------
-- | Description : Magic for controlling inlining
--------------------------------------------------------------------
module Inline (module X) where
import GHC.Magic as X (inline,noinline,lazy,oneShot)
import GHC.Types as X (SPEC(..))
