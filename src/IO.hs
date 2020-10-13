module IO (type (☸),IO,IO_,run) where
import GHC.Magic

run ∷ ∀ (r ∷ RuntimeRep) (o ∷ TYPE r). (Token (☸) → o) → o
run = runRW#
