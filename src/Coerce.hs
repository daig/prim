module Coerce where
import qualified Prelude as GHC
import qualified GHC.Types as GHC

type (=#) = GHC.Coercible

coerce :: forall b a. a =# b => a -> b
coerce = GHC.coerce

coerce# :: forall b a. a -> b
coerce# = GHC.unsafeCoerce#
