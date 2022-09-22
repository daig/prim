{-# language CPP, BangPatterns #-}
module Array.Fold where
import GHC.CString
import Cast
import Cmp
import Action
import Array.Index
import Bits
import Num

type Fold ∷ ∀ {ra} {re} {rr}. T ra → T re → T rr → C
class Fold a e r where fold ∷ a → (e → r) → r

instance Fold (S# Latin1) Char (a → a) where fold = coerce (unpackFoldrCString# @a)
instance Fold (S# UTF8) Char (a → a) where fold = coerce (unpackFoldrCStringUtf8# @a)
#define INST_FOLD_LATIN(A)\
instance Fold (S# Latin1) Char ((a ∷ K A) → a) where {\
    fold (S# s) f r0 = go (Const# (P# s)) r0 \
      where go p@(get → Char8# ch) r = if cast (ch ≡ '\0'#) then r else cast ch `f` go (p +. 1#) r}

INST_FOLD_LATIN(I)
INST_FOLD_LATIN(I8)
INST_FOLD_LATIN(I16)
INST_FOLD_LATIN(I32)
INST_FOLD_LATIN(I64)
INST_FOLD_LATIN(U)
INST_FOLD_LATIN(U8)
INST_FOLD_LATIN(U16)
INST_FOLD_LATIN(U32)
INST_FOLD_LATIN(U64)
INST_FOLD_LATIN(F32)
INST_FOLD_LATIN(F64)
INST_FOLD_LATIN((##))
INST_FOLD_LATIN(ByteArray#)

unpackFoldrCStringUtf9# ∷ S# UTF8 → (Char# → t → t) → t → t
unpackFoldrCStringUtf9# (S# p0) f r0 = go (Const# (P# p0)) r0
  where
    go p r = let !(Char8# ch) = get p
                in if cast (ch ≡ '\0'#) then r else
          let !n = byteCount ch
          in unpackUtf8Char# n ch p `f` go (p +. n) r

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
byteCount :: Char# -> ByteCount
byteCount ch
    | cast (ch ≡ '\x7F'#) = One
    | cast (ch ≡ '\xDF'#) = Two
    | cast (ch ≡ '\xEF'#) = Three
    | T                   = Four

instance Const P_Unbox Char8# +. ByteCount where
  (+.) p = \case One   -> p +. 1#
                 Two   -> p +. 2#
                 Three -> p +. 3#
                 Four  -> p +. 4#

-- | Take the current address, read unicode char of the given size.
-- We obviously want the number of bytes, but we have to read one
-- byte to determine the number of bytes for the current codepoint
-- so we might as well reuse it and avoid a read.
--
-- Side Note: We don't dare to decode all 4 possibilities at once.
-- Reading past the end of the addr might trigger an exception.
-- For this reason we really have to check the width first and only
-- decode after.
{-# INLINE unpackUtf8Char# #-}
unpackUtf8Char# :: ByteCount -> Char# -> Const P_Unbox Char8# -> Char#
unpackUtf8Char# bytes ch (coerce @_ @(ForeignArray# Char8#) → p) =
  case bytes of
    One   → ch
    Two   → cast ( ((cast       ch - 0xC0#) <<#  6##)
                 +  (cast (p ! 1#) - 0x80#          ))
    Three → cast ( ((cast       ch - 0xE0#) <<# 12##)
                 + ((cast (p ! 1#) - 0x80#) <<#  6##)
                 + ( cast (p ! 2#) - 0x80#          ))
    Four  → cast ( ((cast       ch - 0xF0#) <<# 18##)
                 + ((cast (p ! 1#) - 0x80#) <<# 12##)
                 + ((cast (p ! 2#) - 0x80#) <<#  6##)
                 + ( cast (p ! 3#) - 0x80#          ))

{-
{-# INLINE unpackUtf8Char# #-}
unpackUtf8Char# :: ByteCount -> Char# -> P# -> Char#
unpackUtf8Char# bytes ch (addr =
  case bytes of
    One -> ch
    Two ->   (cast @Char# (((cast ch                                           - 0xC0#) <<#  6##) +
                     (cast (indexCharOffAddr# (addr `plusAddr#` 1#) 0#) - 0x80#)))
    Three -> (cast @Char# (((cast ch                                           - 0xE0#) <<# 12##) +
                    ((cast (indexCharOffAddr# (addr `plusAddr#` 1#) 0#) - 0x80#) <<#  6##) +
                     (cast (indexCharOffAddr# (addr `plusAddr#` 2#) 0#) - 0x80#)))
    Four ->  (cast @Char# (((cast ch                                           - 0xF0#) <<# 18##) +
                    ((cast (indexCharOffAddr# (addr `plusAddr#` 1#) 0#) - 0x80#) <<# 12##) +
                    ((cast (indexCharOffAddr# (addr `plusAddr#` 2#) 0#) - 0x80#) <<#  6##) +
                     (cast (indexCharOffAddr# (addr `plusAddr#` 3#) 0#) - 0x80#)))

-}
