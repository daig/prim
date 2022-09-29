--------------------------------------------------------------------
-- | Description : Small Boxed Arrays
--------------------------------------------------------------------
module A.Box.Small (SmallArray#,SmallMutableArray#
                   -- * misc utilities
                   ,module A.Box.Small
                    -- * instance reexports
                    ,module X
                    ) where
import Array as X (Array(..))
import Array.Index as X (type (∈#)(..))
import Array.Copy as X (Copy(..))
import Array.Shrink as X (Shrink(..))

-- | Atomic compare-=and-swap i.e. write the new value if the current value matches the provided expected old value.
-- Implies a full memory barrier.
--
-- _Warning_: This can fail with an unchecked exception.
cas' ∷ ∀ x s. M SmallArray# s x
     → I {- ^ offset -}
     → x {- ^ expected old value -}
     → x {- ^ new value -}
     → ST s (# x | x #) {- ^ @(# newValue | #)@ if successful, or @(# | oldValue #)@ if not -} 
cas' (coerce → m) i x0 x1 = cast (coerce @_ @(ST' s x) (casSmallArray# m i x0 x1))
