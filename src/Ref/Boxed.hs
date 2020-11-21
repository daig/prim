module Ref.Boxed where

type Ref = MutVar#

new ∷ a → ST# s (Ref s a)
new = newMutVar#

read ∷ Ref s a → ST# s a
read = readMutVar#

write ∷ Ref s a → a → ST_# s
write = writeMutVar#

(≡), eq ∷ Ref s a → Ref s a → B#
(≡) = sameMutVar#; eq = sameMutVar#

-- | Modify the contents of a @Ref.Boxed@, returning the previous contents and the result of applying the given function to the previous contents. Note that this isn't strictly speaking the correct type for this function; it should really be MutVar# s a → (a → (a,b)) → State# s → (# State# s, a, (a, b) #), but we don't know about pairs here.
-- 
-- Warning: this can fail with an unchecked exception.


-- | Note that this isn't strictly speaking the correct type for this function;
-- it should really be Ref s a -> (a -> (a,b)) -> ST# s (# a, (a, b) #),
-- but we don't know about pairs here. The pair is lazy and not forced.
--
-- Warning: this can fail with an unchecked exception.
modify2 ∷ Ref s a
       → (a → ab)
       → ST# s (# a, ab #) -- ^ Previous contents and the result of applying the function
modify2 r f s0 = case atomicModifyMutVar2# r f s0 of
  (# s1, old, new #) → (# s1, (# old, new #) #)

modify ∷ Ref s a
        → (a → a)
        → ST# s (# a, a #) -- ^ Previous contents and the result of applying the function
modify r f s0 = case atomicModifyMutVar_# r f s0 of
  (# s1, old, new #) → (# s1, (# old, new #) #)

-- | Compare and swap if the old value matches expected.
cas ∷ Ref s a
    → a -- ^ expected old value
    → a -- ^ new value
    → ST# s (# B#, a #) -- ^ Whether the swap failed, and the actual new value
cas r old new s0 = case casMutVar# r old new s0 of
  (# s1, failed', a #) → (# s1, (# failed', a #) #)
