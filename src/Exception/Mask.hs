--------------------------------------------------------------------
-- | Description : Controlling asynchronous exception delivery
--
-- 
-- NB. there's a bug in here.  If a thread is inside an
-- unsafePerformIO, and inside maskAsyncExceptions# (there is an
-- unmaskAsyncExceptions_ret on the stack), and it is blocked in an
-- interruptible operation, and it receives an exception, then the
-- unsafePerformIO thunk will be updated with a stack object
-- containing the unmaskAsyncExceptions_ret frame.  Later, when
-- someone else evaluates this thunk, the original masking state is
-- not restored.
--------------------------------------------------------------------
module Exception.Mask (State(Unmasked,Uninterruptible,Interruptible)
                      ,async,uninterruptible,state)where
import Prelude hiding (State#)

async,unmask, uninterruptible ∷ IO# a → IO# a
-- | Execute the computation with asynchronous exceptions /blocked/.
-- That is, any thread which attempts to raise an exception in
-- the current thread with 'kill#' will be
-- blocked until async exceptions are unblocked again.  
-- Async Exceptions are re-enabled on exiting the blocked scope.
--
-- Threads created by 'fork' inherit the blocked
-- state from the parent; that is, to start a thread in blocked mode,
-- use @Mask.async (fork ...)@.  This is particularly useful if you need to
-- establish an exception handler in the forked thread before any
-- asynchronous exceptions are received.
-- 
--
--  Async exceptions are masked automatically during
--  the execution of an exception handler.
--
-- Note that if 'kill#' is called with the current thread as the
-- target, the exception will be thrown even if the thread is currently
-- inside 'Exception.Mask.async' or 'Exception.Mask.uninterruptible'.
async           = maskAsyncExceptions#
-- | Unmask async exceptions even inside a masked block.
unmask          = unmaskAsyncExceptions#

uninterruptible = maskUninterruptible#

state ∷ IO# State
state = coerce getMaskingState#

newtype State ∷ T_I where State# ∷ I → State
pattern Unmasked ∷ State
pattern Unmasked = State# 0#
pattern Uninterruptible ∷ State
pattern Uninterruptible = State# 1#
pattern Interruptible ∷ State
pattern Interruptible = State# 2#
{-# complete Unmasked, Uninterruptible, Interruptible #-}
