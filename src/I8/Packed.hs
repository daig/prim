--------------------------------------------------------------------
-- | Description : Native 8-bit Signed Integers
--
-- These types actually pack bytes into a single word, as can be observed
-- by 'RTS.Any.size#' on an unboxed tuple
--------------------------------------------------------------------
module I8.Packed where
import Prelude hiding (I8)

type I8 = Int8#

pattern I8 ∷ I → I8
pattern I8 i ← (extendInt8# → i) where I8 = narrowInt8#

instance (≤) I8 where
  (>) = coerce gtInt8#
  (≥) = coerce geInt8#
  (<) = coerce ltInt8#
  (≤) = coerce leInt8#
instance (≡) I8 where
  (≡) = coerce eqInt8#
  (≠) = coerce neInt8#


(+), (-), (×), add, sub, mul ∷ I8 → I8 → I8
(+) = plusInt8#; add = plusInt8#
(-) = subInt8#; sub y x = x - y
(×) = timesInt8#; mul = timesInt8#
(//), (%%), quot, rem ∷ I8 → I8 → I8
(//) = quotInt8#; quot y x = x // y
(%%) = remInt8#; rem y x = x %% y

quotRem ∷ I8 → I8 → (# I8, I8 #)
quotRem y x = quotRemInt8# x y
