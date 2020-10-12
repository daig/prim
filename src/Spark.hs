module Spark where

par ∷ a → I
par = par#
{-# DEPRECATED par "Use 'spark#' instead" #-}
spark, seq ∷ a → ST s a
spark = spark#
seq = seq#
-- | Get the next spark from the job queue and number of remaining(?) sparks
-- TODO: verify the I argument meaning
get ∷ ST s (# I, a #)
get s0 = case getSpark# s0 of (# s1, n, a #) → (# s1, (# n, a #) #)

num ∷ ST s I
num = numSparks#

