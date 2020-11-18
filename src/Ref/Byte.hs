{-# language PatternSynonyms #-}
module Ref.Byte where

type Ref = Addr#

-- | hack to expose nullAddr#
pattern Null ∷ Ref
pattern Null <- nullAddr# where Null = nullAddr#

add ∷ I → Ref → Ref
add i a = plusAddr# a i

sub ∷ Ref → Ref → I
sub a0 a1 = minusAddr# a1 a0

rem ∷ I → Ref → I
rem i a = remAddr# a i

toI ∷ Ref → I
toI = addr2Int#
fromI ∷ I → Ref
fromI = int2Addr#
{-# DEPRECATED toI, fromI "This operation is strongly deprecated" #-}

gt,ge,lt,le,eq,ne ∷ Ref → Ref → B#
gt y x = gtAddr# x y
ge y x = geAddr# x y
lt y x = ltAddr# x y
le y x = leAddr# x y
eq x y = eqAddr# x y
ne x y = neAddr# x y

toAny ∷ Ref→ (# a #)
toAny = addrToAny#

-- | Must be run on an evaluated value, not a thunk
fromAny# ∷ a → IO# Ref
fromAny# = anyToAddr#
