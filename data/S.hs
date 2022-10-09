--------------------------------------------------------------------
-- | Description : Strings
--------------------------------------------------------------------
module S (S, module S) where
import GHC.CString

type Append ∷ ∀ {rp} {rx}. T rp → T rx → TC
class Append x p | x → p where append ∷ x → p → p
instance Append (S C1) [Char] where append = coerce unpackAppendCString#
instance Append (S C ) [Char] where append = coerce unpackAppendCStringUtf8#
