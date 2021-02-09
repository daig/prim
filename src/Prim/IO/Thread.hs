{-# language PatternSynonyms #-}
module Prim.IO.Thread where
import P
import Prim.IO.Thread.Status


type Id = ThreadId#

-- | The physical capability (Hardware thread) a 'Thread' is running on
type Cap = I

fork ∷ a → IO# Id
fork = fork#
forkOn ∷ Cap → a → IO# Id
forkOn u = forkOn# u
-- | Kill a thread with the given exception (SomeException)
kill# ∷ Id → a → IO_#
kill# = killThread#
yield ∷ IO_#
yield = yield#
here ∷ IO# Id
here = myThreadId#
-- | Label a thread with the given cstring pointer
-- Used in debugging output if the RTS was compiled to support it.
label# ∷ Id → P# → IO_#
label# = labelThread#
bound' ∷ IO# B
bound' = coerce isCurrentThreadBound#
-- TODO: put this somewhere else
noDuplicate ∷ ST_# s
noDuplicate = noDuplicate#
status ∷ Id → IO# (# Status, Cap, B #)
status n s = case threadStatus# n s of
  (# s', status, cap, bound' #) → (# s', (# Status# status, cap, B# bound' #) #)

