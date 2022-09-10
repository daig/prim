--------------------------------------------------------------------
-- | Description : Small Boxed Arrays
--------------------------------------------------------------------
module A.Box.Small (A_Box_Small,A_Box_Small_M
                   -- * misc utilities
                   ,module A.Box.Small
                    -- * instance reexports
                    ,module X
                    ) where
import Array as X (Array(..))
import Array.Index as X (type (∈)(..))
import Array.Copy as X (Copy(..))
import Array.Shrink as X (Shrink(..))
import Array.Atomic as X

-- | Read from the specified index of a 'A_Small'.
-- The result is packaged into an unboxed unary tuple; the result itself is not yet evaluated.
-- Pattern matching on the tuple forces the indexing of the array to happen
-- but does not evaluate the element itself. Evaluating the thunk prevents
-- additional thunks from building up on the heap. Avoiding these thunks, in turn,
-- reduces references to the argument array, allowing it to be garbage collected more promptly.
-- Warning: this can fail with an unchecked exception.
indexLazy# :: forall x. A_Box_Small x -> I -> (# x #)
indexLazy# = coerce (indexSmallArray# @x)
{-# inline indexLazy# #-}
