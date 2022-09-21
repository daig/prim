module Action where
import GHC.CString

type (+.) ∷ ∀ {rp} {rx}. T rp → T rx → Constraint
class p +. x where (+.) ∷ p → x → p

-- |Advances the given address by the given offset (in bytes).
instance P# +. I where (+.) = plusAddr#
instance (P_Unbox U8) +. I where (+.) = coerce plusAddr#
instance (P_Unbox I8) +. I where (+.) = coerce plusAddr#
instance (P_Unbox Char8#) +. I where (+.) = coerce plusAddr#

type (-.) ∷ ∀ {ra} {rx}. T ra → T rx → Constraint
class p +. x ⇒ p -. x where (-.) ∷ p → p → x

-- |Computes the offset (in bytes) required to get from the second to the first argument.
instance P# -. I where (-.) = minusAddr#




type (.+) ∷ ∀ {rp} {rx}. T rp → T rx → Constraint
class x .+ p | x → p where (.+) ∷ x → p → p
instance (S# Latin1) .+ [Char] where (.+) = coerce unpackAppendCString#
instance (S# UTF8) .+ [Char] where (.+) = coerce unpackAppendCStringUtf8#

