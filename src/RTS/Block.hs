module RTS.Block where

delay ∷ I {- ^ microseconds to wait -} → ST_# s
delay = delay#

-- TODO: use a convenience type for file descriptors once you understand how they work.
waitRead,waitWrite ∷ I → ST_# s
waitRead = waitRead#
waitWrite = waitWrite#

