--------------------------------------------------------------------
-- | Description : Blocking concurrent references
--------------------------------------------------------------------
module P.Sync where

empty' ∷ P_Sync a → IO B#
empty' p = coerce (isEmptyMVar# @RealWorld p)

-- | A new empty @Sync.P@
new ∷ IO (P_Sync a)
new = newMVar#

-- | Block until the @Sync.P@ is full, then take the value, leaving it empty.
--
-- If multiple threads are blocked on 'take', only one will wake up per 'write'
-- in FIFO order.
get ∷ P_Sync a → IO a
get = takeMVar#

-- | Without blocking, 'take' the current value if it exists, leaving it empty.
get' ∷ ∀ a. P_Sync a → IO (# (##) | a #) {- ^ The value if the @Sync.P@ was full -}
get' r = cast (coerce @_ @(IO' a) (tryTakeMVar# r))

-- | Block until the @Sync.P@ is full and atomically read the next 'write' value
-- without 'take'ing it, leaving it full.
peek ∷ P_Sync a → IO a
peek = readMVar#

-- | 'read' the current value if it exists but don't 'take' it
-- ,leaving it in the same empty/full state.
peek' ∷ ∀ a. P_Sync a → IO (# (##) | a #) {- ^ The value if the @Sync.P@ was full/unlocked -}
peek' r = cast (coerce @_ @(IO' a) (tryReadMVar# r))

-- | Block until the @Sync.P@ is empty, then write the value, leaving it full.
--
-- If multiple threads are blocked on 'write', only one will wake up per 'take'
-- in FIFO order.
put ∷ P_Sync a → a → IO_
put = putMVar#

-- | Without blocking, try to 'write' to the @Sync.P@ unless it's full, leaving it full.
put' ∷ P_Sync a → a → IO B# {- ^ whether the write succeeded -}
put' p a = coerce (tryPutMVar# p a)
