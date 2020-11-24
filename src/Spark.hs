--------------------------------------------------------------------
-- | Description : Lightweight task concurrency
--
-- See <https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/lw-conc.pdf Lightweight Concurrency Primitives for GHC>
--------------------------------------------------------------------
module Spark where

-- |Mark a thunk for parallel evaluation, adding it to the /spark pool/.
-- When the spark pool is full, new sparks are ignored.
-- Sparks are shared and picked up by idle capabilities.
--
-- See <https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/scheduler#sparks-and-the-par-operator Sparks and the par operator>
par ∷ a → ST# s a
par = spark#
-- | Get the next spark from the job queue and number of remaining sparks
get ∷ ST# s (# I, a #)
get s0 = case getSpark# s0 of (# s1, n, a #) → (# s1, (# n, a #) #)

-- | The number of sparks in the local spark pool.
--
-- For motivation, see https://gitlab.haskell.org/ghc/ghc/-/issues/4167
num ∷ ST# s I
num = numSparks#
