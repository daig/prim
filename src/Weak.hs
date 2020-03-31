module Weak where

type Ptr = Weak#

new :: k -> v -> IO x -> IO (Ptr v)
new = mkWeak# 

newNoFinalizer :: k -> v -> IO (Ptr v)
newNoFinalizer = mkWeakNoFinalizer#

addFinalizer :: Addr -> Addr -> B -> Addr -> Ptr v -> IO B
addFinalizer = addCFinalizerToWeak#

deref :: Ptr v -> IO (Maybe# v)
deref w s0 = case deRefWeak# w s0 of
  (# s1, alive', v #) -> (# s1, (# alive', v #) #)

finalize :: Ptr v -> IO (Maybe# (IO x))
finalize w s0 = case finalizeWeak# w s0 of
  (# s1, alive', finalizer #) -> (# s1, (# alive', finalizer #) #)

touch :: k -> IO_
touch = touch#
