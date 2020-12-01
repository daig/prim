--------------------------------------------------------------------
-- | Description : Primitive boolean type
--------------------------------------------------------------------
module B (B(B#,F,T), module B) where
import I ()

deriving newtype instance (≡) B
deriving newtype instance (≤) B
instance 𝔹 B where
  (∧) = coerce ((∧) @_ @I)
  (∨) = coerce ((∨) @_ @I)
  (⊕) = coerce ((⊕) @_ @I)
  (¬) = (T ⊕)
