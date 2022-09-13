module RTS.Thread (module RTS.Thread, module X) where
import RTS.Thread.Status as X

type Thread = ThreadId#

-- | The physical capability (Hardware thread) a 'Thread' is running on
type Cap = I

fork ∷ ∀ {r} (a ∷ T r). IO a → IO Thread
fork = fork#
forkOn ∷ ∀ {r} (a ∷ T r). Cap → IO a → IO Thread
forkOn u = forkOn# u
-- | Kill a thread with the given exception (SomeException)
kill# ∷ Thread → a → IO_
kill# = killThread#
yield ∷ IO_
yield = yield#
here ∷ IO Thread
here = myThreadId#
-- | Label a thread with the given cstring pointer
-- Used in debugging output if the RTS was compiled to support it.
label# ∷ Thread → S# → IO_
label# = labelThread#
bound' ∷ IO B
bound' = coerce isCurrentThreadBound#
status ∷ Thread → IO (# Status, Cap, B #)
status n s = case threadStatus# n s of
  (# s', status, cap, bound' #) → (# s', (# Status# status, cap, B# bound' #) #)

