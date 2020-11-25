{-# language PatternSynonyms #-}
module Thread where
import P


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
label# ∷ Id → P → IO_#
label# = labelThread#
bound' ∷ IO# B#
bound' = isCurrentThreadBound#
-- TODO: put this somewhere else
noDuplicate ∷ ST_# s
noDuplicate = noDuplicate#
status ∷ Id → IO# (# Status, Cap, B# #)
status n s = case threadStatus# n s of
  (# s', status, cap, bound' #) → (# s', (# Status# status, cap, bound' #) #)

-- * Constants for why_blocked field of a TSO from rts/Constants.h
newtype Status ∷ T_I where Status# ∷ I → Status
pattern Running, BlockedOnMVar, BlockedOnBlackHole, BlockedOnRead, BlockedOnWrite, BlockedOnDelay, BlockedOnSTM, BlockedOnDoProc, BlockedOnCCall, BlockedOnCCall_Interruptible
  ,BlockedOnMsgThrowTo, ThreadMigrating, BlockedOnMVarRead, BlockedOnIOCompletion, ThreadFinished, ThreadDied ∷ Status
pattern Running = Status# 0#
pattern BlockedOnMVar = Status# 1#
pattern BlockedOnBlackHole = Status# 2#
pattern BlockedOnRead = Status# 3#
pattern BlockedOnWrite = Status# 4#
pattern BlockedOnDelay = Status# 5#
pattern BlockedOnSTM = Status# 6#
-- | Win32 only
pattern BlockedOnDoProc = Status# 7#

-- | Only relevant for THREADED_RTS
pattern BlockedOnCCall = Status# 10#
-- | Same as @BlockedOnCCall@ but permit killing the worker thread.
-- Only relevant for THREADED_RTS
pattern BlockedOnCCall_Interruptible = Status# 11#

-- | Involved in a message tsent to tso→msg_cap
pattern BlockedOnMsgThrowTo = Status# 12#

-- | The thread is not on any run queues, but can be woken up by @tryWakeupThread()@
pattern ThreadMigrating = Status# 13#

pattern BlockedOnMVarRead = Status# 14#

-- | Lightweight non-deadlock checked version of MVar.  Used for the why_blocked
-- field of a TSO. Threads blocked for this reason are not forcibly release by
-- the GC, as we expect them to be unblocked in the future based on outstanding IO events.
pattern BlockedOnIOCompletion = Status# 15#
-- TODO: There should be more here. Figure out the numbering

pattern ThreadFinished = Status# 16#
pattern ThreadDied = Status# 17#
