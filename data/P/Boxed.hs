--------------------------------------------------------------------
-- | Description : Mutable References to boxed values
--------------------------------------------------------------------
module P.Boxed where

new ∷ a → ST s (P_Box s a)
new = newMutVar#

read ∷ P_Box s a → ST s a
read = readMutVar#

write ∷ P_Box s a → a → ST_ s
write = writeMutVar#

-- | Modify the contents of a @P.Boxed@, returning the previous contents and the result of applying the given function to the previous contents. Note that this isn't strictly speaking the correct type for this function; it should really be MutVar# s a → (a → (a,b)) → State# s → (# State# s, a, (a, b) #), but we don't know about pairs here.
-- 
-- Warning: this can fail with an unchecked exception.


-- | Note that this isn't strictly speaking the correct type for this function;
-- it should really be @P s a → (a → (a,...)) → ST s (# a, (a,...) #)@,
-- where (a,...) is any record type with a first field of @a@ (eg any sized tuple).
-- That the first field has the right type is not checked, and will be unsafely
-- coerced.
--
-- The return is lazy and not forced. This allows control over evaluation and sharing between
-- The updated value and return value, eg:
--
-- @
-- modify mv \ a → let foo = expensive a in ([3,foo],foo)
-- @
--
-- See <https://mail.haskell.org/pipermail/ghc-devs/2019-October.txt discussion>
-- for "atomicModifyMutVar2"
--
-- Warning: this can fail with an unchecked exception.
modify2 ∷ P_Box s a
       → (a → c)
       → ST s (# a, c #) -- ^ Previous contents and the result of applying the function
modify2 r f s0 = case atomicModifyMutVar2# r f s0 of
  (# s1, old, new #) → (# s1, (# old, new #) #)

modify ∷ P_Box s a
        → (a → a)
        → ST s (# a, a #) -- ^ Previous contents and the result of applying the function
modify r f s0 = case atomicModifyMutVar_# r f s0 of
  (# s1, old, new #) → (# s1, (# old, new #) #)

-- | Compare and swap if the old value matches expected.
cas' ∷ ∀ s a. P_Box s a
    → a -- ^ expected old value
    → a -- ^ new value
    → ST s (# a | a #) -- ^ Whether the swap failed, and the actual new value
cas' r old new = cast (coerce @_ @(ST' s a) (casMutVar# r old new))
{-
cas' r old new s0 = case casMutVar# r old new s0 of
  (# s1, failed', a #) → (# s1, (# B# failed', a #) #)
  -}
