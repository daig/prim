--------------------------------------------------------------------
-- | Description : Software Transactional Memory references
--------------------------------------------------------------------
module P.Async where

--stm ∷ ∀ v. STM# v → IO v
--stm io = atomically# \ s → case io (coerce s) of (# s' , x #) → (# unsafeCoerce# s', x #)

new ∷ a → ST s (P_Async s a)
new = newTVar#

read, readIO ∷ P_Async s a → ST s a
read = readTVar#
-- | Read a @P.STM#@ outside the transaction, without annotating the STM# ledger.
-- Much faster than 'read'.
readIO = readTVarIO#

write ∷ P_Async s a → a → ST_ s
write = writeTVar#
