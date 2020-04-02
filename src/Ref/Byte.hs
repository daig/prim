{-# language PatternSynonyms #-}
module Ref.Byte where

type Ref = Addr#

-- | hack to expose nullAddr#
pattern Null :: Addr
pattern Null <- nullAddr# where Null = nullAddr#

add :: I64 -> Addr -> Addr
add i a = plusAddr# a i

sub :: Addr -> Addr -> I64
sub a0 a1 = minusAddr# a1 a0

rem :: I64 -> Addr -> I64
rem i a = remAddr# a i

toI64 :: Addr -> I64
toI64 = addr2Int#
fromI64 :: I64 -> Addr
fromI64 = int2Addr#
{-# DEPRECATED toI64, fromI64 "This operation is strongly deprecated" #-}

gt,ge,lt,le,eq,ne :: Addr -> Addr -> B
gt y x = gtAddr# x y
ge y x = geAddr# x y
lt y x = ltAddr# x y
le y x = leAddr# x y
eq x y = eqAddr# x y
ne x y = neAddr# x y
