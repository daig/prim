{-# language PatternSynonyms #-}
module Exception.Mask where
import Prelude hiding (State#)

async ∷ IO# a → IO# a
async = maskAsyncExceptions#

uninterruptible ∷ IO# a → IO# a
uninterruptible = maskUninterruptible#

state ∷ IO# State#
state = getMaskingState#

type State# = I

pattern Unmasked ∷ State#
pattern Unmasked = 0#
pattern Uninterruptible ∷ State#
pattern Uninterruptible = 1#
pattern Interruptible ∷ State#
pattern Interruptible = 2#
