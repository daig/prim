module Stable where

type Ptr = StablePtr#

new :: a -> IO (Ptr a)
new = makeStablePtr#

deref :: Ptr a -> IO a
deref = deRefStablePtr#

eq :: Ptr a -> Ptr a -> B
eq = eqStablePtr#
