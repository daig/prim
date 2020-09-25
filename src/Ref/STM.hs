module Ref.STM where

type Ref = TVar#

eq ∷ Ref s a → Ref s a → B
eq = sameTVar#

new ∷ a → ST s (Ref s a)
new = newTVar#

read, readIO ∷ Ref s a → ST s a
read = readTVar#
-- | Read a @Ref.STM@ outside the transaction, without annotating the STM ledger.
-- Much faster than 'read'.
readIO = readTVarIO#

write ∷ Ref s a → a → ST_ s
write = writeTVar#


