--------------------------------------------------------------------
-- | Description : Unboxed Booleans
--------------------------------------------------------------------
module B (I1,B
          -- * Instances reexports
          , module X
          ) where
import Cmp as X
import Bits as X (𝔹(..))
import Cast as X (Cast(..))

type I1 = B
