module Prim.IO.Thread.Status (Status(Status#,Running,Finished,Died,Migrating,BlockedOn), blocked'
                        ,Reason(Reason#,MVar,BlackHole,Read,Write,Delay,STM,DoProc
                        ,CCall,CCall_Interruptible,MsgThrowTo,MVarRead,IOCompletion)
                        ) where
import Prim.B ()

-- * Constants for why_blocked field of a TSO from rts/Constants.h
newtype Status ∷ T_I where Status# ∷ I → Status

newtype Reason ∷ T_I where Reason# ∷ I → Reason
pattern BlockedOn ∷ Reason → Status
pattern BlockedOn r ← (blocked' → (# T , r #)) where BlockedOn (Reason# r) = Status# r
blocked' ∷ Status → Maybe# Reason
blocked' (Status# i) = (# i ≥ 1# ∧ i ≤ 15# ∧ i ≠ 13# , Reason# i #)

pattern Running = Status# 0#
pattern MVar = Reason# 1#
pattern BlackHole = Reason# 2#
pattern Read = Reason# 3#
pattern Write = Reason# 4#
pattern Delay = Reason# 5#
pattern STM = Reason# 6#
-- | Win32 only
pattern DoProc = Reason# 7#

-- | Only relevant for THREADED_RTS
pattern CCall = Reason# 10#
-- | Same as @CCall@ but permit killing the worker thread.
-- Only relevant for THREADED_RTS
pattern CCall_Interruptible = Reason# 11#

-- | Involved in a message tsent to tso→msg_cap
pattern MsgThrowTo = Reason# 12#

-- | The thread is not on any run queues, but can be woken up by @tryWakeupThread()@
pattern Migrating = Status# 13#

pattern MVarRead = Reason# 14#

-- | Lightweight non-deadlock checked version of MVar.  Used for the why_blocked
-- field of a TSO. Threads blocked for this reason are not forcibly release by
-- the GC, as we expect them to be unblocked in the future based on outstanding IO events.
pattern IOCompletion = Reason# 15#
-- TODO: There should be more here. Figure out the numbering

pattern Finished = Status# 16#
pattern Died = Status# 17#
