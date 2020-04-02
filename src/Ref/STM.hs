module Ref.STM where

type Ref = TVar#

eq :: Ref s a -> Ref s a -> B
eq = sameTVar#

new :: a -> ST s (Ref s a)
new = newTVar#

read, readIO :: Ref s a -> ST s a
read = readTVar#
-- | Read a @Ref.STM@ outside the transaction, without annotating the STM ledger.
-- Much faster than 'read'.
readIO = readTVarIO#

write :: Ref s a -> a -> ST_ s
write = writeTVar#


-- * General STM operations
-- TODO: maybe split this up into its own module

atomically :: STM a -> IO a
atomically = atomically#

-- | Retry execution of the current memory transaction because it has seen values in TVars which mean that it should not continue (e.g. the TVars represent a shared buffer that is now empty).
-- The implementation may block the thread until one of the TVars that it has read from has been updated.
retry :: STM a
retry = retry#

catch :: STM a -> (e -> STM a) -> STM a
catch = catchSTM#

catch_ :: STM a -> STM a -> STM a
catch_ = catchRetry#

