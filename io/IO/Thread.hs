{-# language PartialTypeSignatures #-}
{-# language LinearTypes #-}
module IO.Thread where
import {-# source #-} I (I)
import {-# source #-} B
import P
import IO
import IO.Thread.Status


type Id = ThreadId#

-- | The physical capability (Hardware thread) a 'Thread' is running on
type Cap = I

fork ∷ a ⊸ IO Id
fork = unsafeCoerce# fork#

forkOn ∷ Cap ⊸ a ⊸ IO Id
forkOn = unsafeCoerce# forkOn#
-- | Kill a thread with the given exception (SomeException)
kill# ∷ Id ⊸ a ⊸ IO_
kill# = unsafeCoerce# killThread#
yield ∷ IO_
yield = unsafeCoerce# yield#
here ∷ IO Id
here = unsafeCoerce# myThreadId#
-- | Label a thread with the given cstring pointer
-- Used in debugging output if the RTS was compiled to support it.
label# ∷ Id ⊸ P# ⊸ IO_
label# = unsafeCoerce# labelThread#
bound' ∷ IO B
bound' = unsafeCoerce# isCurrentThreadBound#
-- TODO: put this somewhere else
noDuplicate ∷ IO_
noDuplicate = unsafeCoerce# noDuplicate#
status ∷ Id ⊸ IO (# Status, Cap, B #)
status = unsafeCoerce# \ n s → case threadStatus# n s of
  (# s', status, cap, bound' #) → (# s', (# Status# status, cap, B# bound' #) #)
