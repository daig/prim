--------------------------------------------------------------------
-- | Description : Primitive boolean type
--------------------------------------------------------------------
module B (B(B#,F,T), module B) where
import Num
import I ()


deriving newtype instance (⊕) B
instance (¬) B where (¬) = (T ⊕)
