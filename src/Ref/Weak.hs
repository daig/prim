module Ref.Weak where
import qualified Ref.Byte as Byte

type Ref = Weak#

new ∷ k → v → IO# x → IO# (Ref v)
new = mkWeak# 

newNoFinalizer ∷ k → v → IO# (Ref v)
newNoFinalizer = mkWeakNoFinalizer#

addFinalizer ∷ Byte.Ref → Byte.Ref → B# → Byte.Ref → Ref v → IO# B#
addFinalizer = addCFinalizerToWeak#

deref ∷ Ref v → IO# (Maybe# v)
deref w s0 = case deRefWeak# w s0 of
  (# s1, alive', v #) → (# s1, (# alive', v #) #)

finalize ∷ Ref v → IO# (Maybe#  (IO# x))
finalize w s0 = case finalizeWeak# w s0 of
  (# s1, alive', finalizer #) → (# s1, (# alive', finalizer #) #)

touch ∷ k → IO_#
touch = touch#
