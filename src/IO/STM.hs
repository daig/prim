module IO.STM where
import IO
import GHC.Prim

type STM# (a ∷ TYPE r) = IO# a

run ∷ STM# a → IO# a
run = atomically#

-- | Retry execution of the current memory transaction because it has seen values in TVars which mean that it should not continue (e.g. the TVars represent a shared buffer that is now empty).
-- The implementation may block the thread until one of the TVars that it has read from has been updated.
retry ∷ STM# a
retry = retry#

catch ∷ STM# a → (e → STM# a) → STM# a
catch = catchSTM#

catch_ ∷ STM# a → STM# a → STM# a
catch_ = catchRetry#
