{-# OPTIONS_GHC -Wno-deprecations #-}
{-# language BangPatterns #-}
module String.C.UTF8 where
import qualified String
import String.C
import Prelude hiding (Char)
import Stock.Char (Char(C#))
import B
import Char ((≤),(≡),toI,fromI)
import I ((+),(-),shiftL#)
import P.Char8 ((!#))

-- There's really no point in inlining this for the same reasons as
-- unpack. See Note [Inlining unpack#] above for details.
unpack# ∷ S → String.List
{- NOINLINE CONLIKE unpack# #-}
unpack# addr = unpack 0# where
    -- We take care to strictly evaluate the character decoding as
    -- index# is marked with the can_fail flag and
    -- consequently GHC won't evaluate the expression unless it is absolutely
    -- needed.
    unpack nh
      | B# (ch ≡ '\0'#  ) = []
      | B# (ch ≤ '\x7F'#) = C# ch : unpack (nh + 1#)
      | B# (ch ≤ '\xDF'#) =
          let !c = C# (fromI (shiftL# 6# (toI ch            - 0xC0#) +
                               (toI (addr !# (nh + 1#)) - 0x80#)))
          in c : unpack (nh + 2#)
      | B# (ch ≤ '\xEF'#) =
          let !c = C# (fromI (shiftL# 12# (toI ch                      - 0xE0#) +
                              shiftL#  6# (toI (addr !# (nh + 1#)) - 0x80#) +
                                          (toI (addr !# (nh + 2#)) - 0x80#)))
          in c : unpack (nh + 3#)
      | T                 =
          let !c = C# (fromI (shiftL# 18# (toI ch                      - 0xF0#) +
                              shiftL# 12# (toI (addr !# (nh + 1#)) - 0x80#) +
                              shiftL#  6# (toI (addr !# (nh + 2#)) - 0x80#) +
                               (toI (addr !# (nh + 3#)) - 0x80#)))
          in c : unpack (nh + 4#)
      where !ch = addr !# nh
