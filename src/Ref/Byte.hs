{-# language PatternSynonyms #-}
module Ref.Byte where

type Ref = Addr#

-- | hack to expose nullAddr#
pattern Null :: Ref
pattern Null <- nullAddr# where Null = nullAddr#

add :: I64 -> Ref -> Ref
add i a = plusAddr# a i

sub :: Ref -> Ref -> I64
sub a0 a1 = minusAddr# a1 a0

rem :: I64 -> Ref -> I64
rem i a = remAddr# a i

toI64 :: Ref -> I64
toI64 = addr2Int#
fromI64 :: I64 -> Ref
fromI64 = int2Addr#
{-# DEPRECATED toI64, fromI64 "This operation is strongly deprecated" #-}

gt,ge,lt,le,eq,ne :: Ref -> Ref -> B
gt y x = gtAddr# x y
ge y x = geAddr# x y
lt y x = ltAddr# x y
le y x = leAddr# x y
eq x y = eqAddr# x y
ne x y = neAddr# x y

toAny :: Ref-> (# a #)
toAny = addrToAny#

-- | Must be run on an evaluated value, not a thunk
fromAny# :: a -> IO Ref
fromAny# = anyToAddr#
