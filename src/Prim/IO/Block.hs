module Prim.IO.Block where

delay ∷ I {- ^ microseconds to wait -} → ST_ s
delay = delay#

-- TODO: use a convenience type for file descriptors once you understand how they work.
waitRead,waitWrite ∷ I {- ^ file descriptor -} → ST_ s
-- |Block until input is available on specified file descriptor.
waitRead = waitRead#
-- |Block until output is possible on specified file descriptor.
waitWrite = waitWrite#

