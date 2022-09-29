{-# language InstanceSigs #-}
module Array.Boxed where
import Cast

type BoxedArray ∷ ∀ {l}. (T# l → T_) → C
class BoxedArray a where

    -- | Read from the specified index of a the array
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
    (!~) ∷ a x → I → (# x #)
-- | Atomic compare-=and-swap i.e. write the new value if the current value matches the provided expected old value.
-- Implies a full memory barrier.
--
-- _Warning_: This can fail with an unchecked exception.
    cas' ∷ ∀ x s. M a s x
         → I {- ^ offset -}
         → x {- ^ expected old value -}
         → x {- ^ new value -}
         → ST s (# x | x #) {- ^ @(# newValue | #)@ if successful, or @(# | oldValue #)@ if not -} 
instance BoxedArray SmallArray# where
  (!~) = indexSmallArray#
  cas' ∷ ∀ x s. M SmallArray# s x
       → I → x → x → ST s (# x | x #)
  cas' (coerce → m) i x0 x1 = cast (coerce @_ @(ST' s x) (casSmallArray# m i x0 x1))
instance BoxedArray Array# where
  (!~) = indexArray#
  cas' ∷ ∀ x s. M Array# s x
       → I → x → x → ST s (# x | x #)
  cas' (coerce → m) i x0 x1 = cast (coerce @_ @(ST' s x) (casArray# m i x0 x1))
