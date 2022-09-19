module Action where

type (∔) ∷ ∀ {rp} {rx}. T rp → T rx → Constraint
class p ∔ x | p → x where (∔) ∷ p → x → p

type (߸) ∷ ∀ {ra} {rx}. T ra → T rx → Constraint
class p ∔ x ⇒ p ߸ x where (߸) ∷ p → p → x

type (⨰) ∷ ∀ {ra} {rx}. T ra → T rx → Constraint
class p ⨰ x | p → x where (⨰) ∷ p → x → p

type (.=.) ∷ ∀ {ra} {rx}. T ra → T rx → Constraint
class a .=. b where (.=.) ∷ a → b → B#
class a .<. b where
  (.<.),(.≤.),(.>.),(.≥.) ∷ a → b → B#





-- |Advances the given address by the given offset (in bytes).
instance P# ∔ I where (∔) = plusAddr#
-- |Computes the offset (in bytes) required to get from the second to the first argument.
instance P# ߸ I where (߸) = minusAddr#
