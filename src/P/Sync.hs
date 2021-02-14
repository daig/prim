{-# language ScopedTypeVariables, TypeApplications #-}
--------------------------------------------------------------------
-- | Description : Blocking concurrent references
--------------------------------------------------------------------
module P.Sync where
import Prelude hiding (P)

-- | A synchronising variable, used for communication between concurrent threads.
-- It can be thought of as a box, which may be empty or full.
--
-- The RTS implementation is really an abstraction for
-- connecting 'take' and 'write' calls between threads
type P = MVar#

eq ∷ P s a → P s a → B
eq p q = B (sameMVar# p q)

empty' ∷ P s a → ST s B
empty' p = coerce (isEmptyMVar# p)

-- | A new empty @Sync.P@
new ∷ ST s (P s a)
new = newMVar#

-- | Block until the @Sync.P@ is full, then take the value, leaving it empty.
--
-- If multiple threads are blocked on 'take', only one will wake up per 'write'
-- in FIFO order.
take ∷ P s a → ST s a
take = takeMVar#

-- | Without blocking, 'take' the current value if it exists, leaving it empty.
take' ∷ P s a → ST s (Maybe# a) {- ^ The value if the @Sync.P@ was full -}
take' r s0 = case tryTakeMVar# r s0 of
  (# s1, full', a #) → (# s1, (# B full', a #) #)

-- | Block until the @Sync.P@ is full and atomically read the next 'write' value
-- without 'take'ing it, leaving it full.
read ∷ P s a → ST s a
read = readMVar#

-- | 'read' the current value if it exists but don't 'take' it
-- ,leaving it in the same empty/full state.
read' ∷ P s a → ST s (Maybe# a) {- ^ The value if the @Sync.P@ was full/unlocked -}
read' r s0 = case tryReadMVar# r s0 of
  (# s1, full', a #) → (# s1, (# B full', a #) #)

-- | Block until the @Sync.P@ is empty, then write the value, leaving it full.
--
-- If multiple threads are blocked on 'write', only one will wake up per 'take'
-- in FIFO order.
write ∷ P s a → a → ST_ s
write = putMVar#

-- | Without blocking, try to 'write' to the @Sync.P@ unless it's full, leaving it full.
write' ∷ P s a → a → ST s B {- ^ whether the write succeeded -}
write' p a = coerce (tryPutMVar# p a)
