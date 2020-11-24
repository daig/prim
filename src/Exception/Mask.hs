module Exception.Mask (State(Unmasked,Uninterruptible,Interruptible)
                      ,async,uninterruptible,state)where
import Prelude hiding (State#)

async ∷ IO# a → IO# a
async = maskAsyncExceptions#

uninterruptible ∷ IO# a → IO# a
uninterruptible = maskUninterruptible#

state ∷ IO# State
state = coerce getMaskingState#

newtype State ∷ TYPE IntRep where State# ∷ I → State
pattern Unmasked ∷ State
pattern Unmasked = State# 0#
pattern Uninterruptible ∷ State
pattern Uninterruptible = State# 1#
pattern Interruptible ∷ State
pattern Interruptible = State# 2#
{-# complete Unmasked, Uninterruptible, Interruptible #-}
