module Exception where

-- | Can only catch values with an @Exception@ instance.
catch# ∷ IO a → (e → IO a) → IO a
catch# = Prelude.catch#

-- | Raising values other than type @SomeException@ leads to segfault
raise# ∷ ∀ (r ∷ RuntimeRep) (a ∷ TYPE r) e. e → a
raise# = Prelude.raise#
-- | Raising values other than type @SomeException@ leads to segfault
raiseIO# ∷ e → IO a
raiseIO# = Prelude.raiseIO#
