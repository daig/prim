--------------------------------------------------------------------
-- | Description : Software Transactional Memory references
--------------------------------------------------------------------
module P.Async where
import Coerce

--stm ∷ ∀ v. STM# v → IO v
--stm io = atomically# \ s → case io (coerce s) of (# s' , x #) → (# unsafeCoerce# s', x #)

new ∷ ∀ a. a → STM (Async# a)
new = coerce (newTVar# @a)

read ∷ ∀ a. Async# a → STM a
read = coerce (readTVar# @_ @a)
-- | Read a @P.STM#@ outside the transaction, without annotating the STM# ledger.
-- Much faster than 'read'.
readIO ∷ ∀ a. Async# a → IO a
readIO = unsafeCoerce# (readTVarIO# @RealWorld @a)

write ∷ ∀ a. Async# a → a → STM_ 
write = coerce (writeTVar# @_ @a)
