--------------------------------------------------------------------
-- | Description : Software Transactional Memory
--------------------------------------------------------------------
module STM (module STM) where
import Coerce



-- | Retry execution of the current memory transaction because it has seen values in TVars which mean that it should not continue (e.g. the TVars represent a shared buffer that is now empty).
-- The implementation may block the thread until one of the TVars that it has read from has been updated.
retry ∷ ∀ a. STM a
retry = unsafeCoerce# (retry# @a)

-- | Catch any 'raiseIO' thrown in the first transaction, resetting and handling with the second.
catch ∷ ∀ a e. STM a → (e → STM a) → STM a
catch = unsafeCoerce# (catchSTM# @a @e)

-- | If the first transaction attempts to 'retry', reset and run the second.
catch_ ∷ ∀ a. STM a → STM a → STM a
catch_ = unsafeCoerce# (catchRetry# @a)
