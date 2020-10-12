module Ref.Stable where

type Ref = StablePtr#

new ∷ a → IO (Ref a)
new = makeStablePtr#

deref ∷ Ref a → IO a
deref = deRefStablePtr#

eq ∷ Ref a → Ref a → B#
eq = eqStablePtr#
