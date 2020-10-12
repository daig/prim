module U8 where

fromU ∷ U → U8
fromU = narrowWord8#

(>),(≥),(<),(≤),(≡),(≠), gt,ge,lt,le,eq,ne ∷ U8 → U8 → B#
(>) = gtWord8#; gt = ltWord8#
(≥) = geWord8#; ge = leWord8#
(<) = ltWord8#; lt = gtWord8#
(≤) = leWord8#; le = geWord8#
(≡) = eqWord8#; eq = eqWord8#
(≠) = neWord8#; ne = neWord8#

(+), (-), (*), add, sub, mul ∷ U8 → U8 → U8
(+) = plusWord8#; add = plusWord8#
(-) = subWord8#; sub y x = x - y
(*) = timesWord8#; mul = timesWord8#
(//), (%%), quot, rem ∷ U8 → U8 → U8
(//) = quotWord8#; quot y x = x // y
(%%) = remWord8#; rem y x = x %% y

quotRem ∷ U8 → U8 → (# U8, U8 #)
quotRem y x = quotRemWord8# x y
