{-# language PatternSynonyms #-}
module Thread where
import qualified Ref

type Id = ThreadId#

-- | The physical capability (Hardware thread) a 'Thread' is running on
type Cap = I64

fork :: a -> IO Id
fork = fork#
forkOn :: Cap -> a -> IO Id
forkOn u = forkOn# u
-- | Kill a thread with the given exception (toException)
kill# :: Id -> a -> IO_
kill# = killThread#
yield :: IO_
yield = yield#
here :: IO Thread
here = myThreadId#
-- | Label a thread with the given cstring pointer
label# :: Id -> Ref.Byte -> IO_
label# = labelThread#
bound' :: IO B
bound' = isCurrentThreadBound#
-- TODO: put this somewhere else
noDuplicate :: ST_ s
noDuplicate = noDuplicate#
status :: Id -> IO (# Status, Cap, B #)
status n s = case threadStatus# n s of
  (# s', status, cap, bound' #) -> (# s', (# status, cap, bound' #) #)

-- * Constants for why_blocked field of a TSO from rts/Constants.h
type Status = Int#
pattern Running = 0#
pattern BlockedOnMVar = 1#
pattern BlockedOnBlackHole = 2#
pattern BlockedOnRead = 3#
pattern BlockedOnWrite = 4#
pattern BlockedOnDelay = 5#
pattern BlockedOnSTM = 6#
-- | Win32 only
pattern BlockedOnDoProc = 7#

-- | Only relevant for THREADED_RTS
pattern BlockedOnCCall = 10#
-- | Same as @BlockedOnCCall@ but permit killing the worker thread.
-- Only relevant for THREADED_RTS
pattern BlockedOnCCall_Interruptible = 11#

-- | Involved in a message tsent to tso->msg_cap
pattern BlockedOnMsgThrowTo = 12#

-- | The thread is not on any run queues, but can be woken up by @tryWakeupThread()@
pattern ThreadMigrating = 13#

-- TODO: There should be more here, like ThreadFinished. Figure out the numbering


