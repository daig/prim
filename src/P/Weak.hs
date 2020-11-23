module P.Weak where
import qualified P.Byte as Byte

type P = Weak#

new ∷ k → v → IO# x → IO# (P v)
new = mkWeak# 

newNoFinalizer ∷ k → v → IO# (P v)
newNoFinalizer = mkWeakNoFinalizer#

addFinalizer ∷ Byte.P → Byte.P → B# → Byte.P → P v → IO# B#
addFinalizer = addCFinalizerToWeak#

deref ∷ P v → IO# (Maybe# v)
deref w s0 = case deRefWeak# w s0 of
  (# s1, alive', v #) → (# s1, (# alive', v #) #)

finalize ∷ P v → IO# (Maybe#  (IO# x))
finalize w s0 = case finalizeWeak# w s0 of
  (# s1, alive', finalizer #) → (# s1, (# alive', finalizer #) #)

touch ∷ k → IO_#
touch = touch#
