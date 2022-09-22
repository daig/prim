--------------------------------------------------------------------
-- | Description : Boxed Arrays
--------------------------------------------------------------------
module A.Box (A_Box,A_Box_M
               -- * misc utilities
               ,module A.Box
               -- * instance reexports
               ,module X
               ) where
import Array as X (Array(..))
import Array.Index as X (type (∈)(..))
import Array.Copy as X (Copy(..))

-- | Read from the specified index of a 'A_Box'.
-- The result is packaged into an unboxed unary tuple; the result itself is not yet evaluated.
-- Pattern matching on the tuple forces the indexing of the array to happen
-- but does not evaluate the element itself.
--
-- Evaluating the thunk prevents additional thunks from building up on the heap.
-- Avoiding these thunks, in turn, reduces references to the argument array,
-- allowing it to be garbage collected more promptly.
-- For strict indexing, use '(!)'
--
-- Warning: this can fail with an unchecked exception.
indexLazy# ∷ ∀ x. A_Box x → I → (# x #)
indexLazy# = coerce (indexArray# @x)
{-# inline indexLazy# #-}

-- | Atomic compare-=and-swap i.e. write the new value if the current value matches the provided expected old value.
-- Implies a full memory barrier.
--
-- _Warning_: This can fail with an unchecked exception.
cas' ∷ ∀ x s. M (A_Box x) s
     → I {- ^ offset -}
     → x {- ^ expected old value -}
     → x {- ^ new value -}
     → ST s (# x | x #) {- ^ @(# newValue | #)@ if successful, or @(# | oldValue #)@ if not -} 
cas' (coerce → m) i x0 x1 = cast (coerce @_ @(State# s → (# State# s, B#, x #)) (casArray# m i x0 x1))
