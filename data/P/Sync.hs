--------------------------------------------------------------------
-- | Description : Blocking concurrent references
--------------------------------------------------------------------
module P.Sync where

empty' ∷ P_Sync s a → ST s B
empty' p = coerce (isEmptyMVar# p)

-- | A new empty @Sync.P@
new ∷ ST s (P_Sync s a)
new = newMVar#

-- | Block until the @Sync.P@ is full, then take the value, leaving it empty.
--
-- If multiple threads are blocked on 'take', only one will wake up per 'write'
-- in FIFO order.
get ∷ P_Sync s a → ST s a
get = takeMVar#

-- | Without blocking, 'take' the current value if it exists, leaving it empty.
get' ∷ P_Sync s a → ST s (Maybe# a) {- ^ The value if the @Sync.P@ was full -}
get' r s0 = case tryTakeMVar# r s0 of
  (# s1, full', a #) → (# s1, (# B# full', a #) #)

-- | Block until the @Sync.P@ is full and atomically read the next 'write' value
-- without 'take'ing it, leaving it full.
peek ∷ P_Sync s a → ST s a
peek = readMVar#

-- | 'read' the current value if it exists but don't 'take' it
-- ,leaving it in the same empty/full state.
peek' ∷ P_Sync s a → ST s (Maybe# a) {- ^ The value if the @Sync.P@ was full/unlocked -}
peek' r s0 = case tryReadMVar# r s0 of
  (# s1, full', a #) → (# s1, (# B# full', a #) #)

-- | Block until the @Sync.P@ is empty, then write the value, leaving it full.
--
-- If multiple threads are blocked on 'write', only one will wake up per 'take'
-- in FIFO order.
put ∷ P_Sync s a → a → ST_ s
put = putMVar#

-- | Without blocking, try to 'write' to the @Sync.P@ unless it's full, leaving it full.
put' ∷ P_Sync s a → a → ST s B {- ^ whether the write succeeded -}
put' p a = coerce (tryPutMVar# p a)