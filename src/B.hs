--------------------------------------------------------------------
-- | Description : Primitive boolean type
--------------------------------------------------------------------
module B (B(B#,F,T), module B) where
import Num
import I ()


instance 𝔹 B where
  (∧) = coerce ((∧) @_ @I)
  (∨) = coerce ((∨) @_ @I)
  (⊕) = coerce ((⊕) @_ @I)
  (¬) = (T ⊕)
