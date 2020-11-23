module P.STM where

type P = TVar#

(≡), eq ∷ P s a → P s a → B#
(≡) = sameTVar#; eq = sameTVar#

new ∷ a → ST# s (P s a)
new = newTVar#

read, readIO ∷ P s a → ST# s a
read = readTVar#
-- | Read a @P.STM#@ outside the transaction, without annotating the STM# ledger.
-- Much faster than 'read'.
readIO = readTVarIO#

write ∷ P s a → a → ST_# s
write = writeTVar#


