{-# language CPP, BangPatterns #-}
module Array.Fold where
import GHC.CString
import Cast
import Cmp
import Action
import Array.Index
import Bits
import Num
import Var
import Array.Map


unpackFoldrCStringUtf9# ∷ S# C# → (C# → t → t) → t → t
unpackFoldrCStringUtf9# (S# p0) f r0 = go (ConstAddr# @C1# p0) r0
  where
    go p r = let !(C1# ch) = p!0#
                in if ch == '\0'# then r else
          let !n = byteCount ch
          in unpackUtf8C# n ch p `f` go (p +. n) r

------------------------------
--- UTF8 decoding utilities
------------------------------
--
-- These functions make explicit the logic that was originally
-- part of unpackCStringUtf8. Since we want the same support for ascii
-- and non-ascii a variety of functions needs the same logic. Instead
-- of C&P'in the decoding logic all over we have it here once, and then
-- force GHC to inline it.
--
-- All the overhead of the Bytes argument and calls goes away once all is
-- said and done. And what remains is readable code in Haskell land and
-- performant code in the resulting binary.

data ByteCount = One | Two | Three | Four
{-# INLINE byteCount #-}
byteCount ∷ C# → ByteCount
byteCount ch
    | ch == '\x7F'# = One
    | ch == '\xDF'# = Two
    | ch == '\xEF'# = Three
    | T                   = Four

instance ForeignArray# C1# +. ByteCount where
  (+.) p = \case One   → p +. 1#
                 Two   → p +. 2#
                 Three → p +. 3#
                 Four  → p +. 4#

-- | Take the current address, read unicode char of the given size.
-- We obviously want the number of bytes, but we have to read one
-- byte to determine the number of bytes for the current codepoint
-- so we might as well reuse it and avoid a read.
--
-- Side Note: We don't dare to decode all 4 possibilities at once.
-- Reading past the end of the addr might trigger an exception.
-- For this reason we really have to check the width first and only
-- decode after.
{-# INLINE unpackUtf8C# #-}
unpackUtf8C# ∷ ByteCount → C# → ForeignArray# C1# → C#
unpackUtf8C# bytes ch (coerce @_ @(ForeignArray# C1#) → p) =
  case bytes of
    One   →                     ch
    Two   → cast ( ((cast       ch - 0xC0#) <<#  6##)
                 +  (cast (p ! 1#) - 0x80#          ))
    Three → cast ( ((cast       ch - 0xE0#) <<# 12##)
                 + ((cast (p ! 1#) - 0x80#) <<#  6##)
                 + ( cast (p ! 2#) - 0x80#          ))
    Four  → cast ( ((cast       ch - 0xF0#) <<# 18##)
                 + ((cast (p ! 1#) - 0x80#) <<# 12##)
                 + ((cast (p ! 2#) - 0x80#) <<#  6##)
                 + ( cast (p ! 3#) - 0x80#          ))
