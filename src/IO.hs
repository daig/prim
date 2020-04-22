module IO where
--import GHC.Types (TYPE)
import GHC.Magic

run :: forall (r :: RuntimeRep) (o :: TYPE r). (Token (☸) -> o) -> o
run = runRW#
