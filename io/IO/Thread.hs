--------------------------------------------------------------------
-- | Description : Explicit Multithreading
--------------------------------------------------------------------
module IO.Thread where
import IO.Thread.Status


type Id = ThreadId#

-- | The physical capability (Hardware thread) a 'Thread' is running on
type Cap = I

fork ∷ IO a → IO Id
fork = fork#

forkOn ∷ Cap → IO a → IO Id
forkOn = forkOn#
-- | Kill a thread with the given exception (SomeException)
kill# ∷ Id → a → IO_
kill# = killThread#
yield ∷ IO_
yield = yield#
here ∷ IO Id
here = myThreadId#
-- | Label a thread with the given cstring pointer
-- Used in debugging output if the RTS was compiled to support it.
label# ∷ Id → S x → IO_
label# = coerce labelThread#
bound' ∷ IO B#
bound' = coerce isCurrentThreadBound#
status ∷ Id → IO (# Status, Cap, B# #)
status n s = case threadStatus# n s of
  (# s', status, cap, bound' #) → (# s', (# Status# status, cap, B# bound' #) #)
