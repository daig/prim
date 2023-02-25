--------------------------------------------------------------------
-- | Description : Existential type of all exceptions.
--------------------------------------------------------------------
module IO.Exception.Some (E, module X) where
import GHC.Exception as X (Exception(..),SomeException(..))
type E = Exception
