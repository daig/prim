{-# language BangPatterns #-}
module String.C.UTF8 where
import qualified String
import String.C
import Prelude hiding (Char)
import GHC.Types (Char(..),isTrue#,Bool(..))

-- There's really no point in inlining this for the same reasons as
-- unpack. See Note [Inlining unpack#] above for details.
unpack# :: S -> String.Linked
{-# NOINLINE CONLIKE unpack# #-}
unpack# addr
  = unpack 0#
  where
    -- We take care to strictly evaluate the character decoding as
    -- indexCharOffAddr# is marked with the can_fail flag and
    -- consequently GHC won't evaluate the expression unless it is absolutely
    -- needed.
    unpack nh
      | isTrue# (ch `eqChar#` '\0'#  ) = []
      | isTrue# (ch `leChar#` '\x7F'#) = C# ch : unpack (nh +# 1#)
      | isTrue# (ch `leChar#` '\xDF'#) =
          let !c = C# (chr# (((ord# ch                                  -# 0xC0#) `uncheckedIShiftL#`  6#) +#
                              (ord# (indexCharOffAddr# addr (nh +# 1#)) -# 0x80#)))
          in c : unpack (nh +# 2#)
      | isTrue# (ch `leChar#` '\xEF'#) =
          let !c = C# (chr# (((ord# ch                                  -# 0xE0#) `uncheckedIShiftL#` 12#) +#
                             ((ord# (indexCharOffAddr# addr (nh +# 1#)) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                              (ord# (indexCharOffAddr# addr (nh +# 2#)) -# 0x80#)))
          in c : unpack (nh +# 3#)
      | True                           =
          let !c = C# (chr# (((ord# ch                                  -# 0xF0#) `uncheckedIShiftL#` 18#) +#
                             ((ord# (indexCharOffAddr# addr (nh +# 1#)) -# 0x80#) `uncheckedIShiftL#` 12#) +#
                             ((ord# (indexCharOffAddr# addr (nh +# 2#)) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                              (ord# (indexCharOffAddr# addr (nh +# 3#)) -# 0x80#)))
          in c : unpack (nh +# 4#)
      where
        !ch = indexCharOffAddr# addr nh

