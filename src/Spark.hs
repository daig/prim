module Spark where

par :: a -> I64
par = par#
spark, seq :: a -> ST s a
spark = spark#
seq = seq#
-- | Get the next spark from the job queue and number of remaining(?) sparks
-- TODO: verify the I64 argument meaning
get :: ST s (# I64, a #)
get s0 = case getSpark# s0 of (# s1, n, a #) -> (# s1, (# n, a #) #)

num :: ST s I64
num = numSparks#

