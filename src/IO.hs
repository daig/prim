module IO (Token,ST#,ST_#,type (☸),IO#,IO_#,run) where
import GHC.Magic

run ∷ ∀ (r ∷ RuntimeRep) (o ∷ T_ r). (Token (☸) → o) → o
run = runRW#
