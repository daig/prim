{-# language BangPatterns #-}
module String.C.Latin where
import qualified String
import String.C
import Prelude hiding (Char)
import GHC.Types (Char(..),isTrue#,Bool(..))

--------------------------------------------------------------------------
-- Unpacking C strings
-----------------------------------------------------------------------------

-- This code is needed for virtually all programs, since it's used for
-- unpacking the strings of error messages.

-- Used to be in GHC.Base, but was moved to ghc-prim because the new generics
-- stuff uses Strings in the representation, so to give representations for
-- ghc-prim types we need unpack#

{- Note [Inlining unpack#]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There's really no point in ever inlining things like unpack# as the loop
doesn't specialise in an interesting way and we can't deforest the list
constructors (we'd want to use foldr# for this). Moreover, it's
pretty small, so there's a danger that it'll be inlined at every literal, which
is a waste.

Moreover, inlining early may interfere with a variety of rules that are supposed
to match unpack#,

 * BuiltInRules in PrelRules.hs; e.g.
       eqString (unpack# (Lit s1)) (unpack# (Lit s2)
          = s1 == s2

 * unpacking rules; e.g. in GHC.Base,
       unpack# a
          = build (foldr# a)

 * stream fusion rules; e.g. in the `text` library,
       unstream (S.map safe (S.streamList (GHC.unpack# a)))
          = unpack# a

Moreover, we want to make it CONLIKE, so that:

* the rules in PrelRules will fire when the string is let-bound.
  E.g. the eqString rule in PrelRules
   eqString (unpack# (Lit s1)) (unpack# (Lit s2) = s1==s2

* exprIsConApp_maybe will see the string when we have
     let x = unpack# "foo"#
     ...(case x of algs)...

All of this goes for unpackUtf8# too.
-}


unpack# ∷ S → String.List
{-# NOINLINE CONLIKE unpack# #-}
unpack# addr
  = unpack 0#
  where
    unpack nh
      | isTrue# (ch `eqChar#` '\0'#) = []
      | True                         = C# ch : unpack (nh +# 1#)
      where
        !ch = indexCharOffAddr# addr nh

unpackAppend# ∷ S → String.List → String.List
{-# NOINLINE unpackAppend# #-}
     -- See the NOINLINE note on unpack#
unpackAppend# addr rest
  = unpack 0#
  where
    unpack nh
      | isTrue# (ch `eqChar#` '\0'#) = rest
      | True                         = C# ch : unpack (nh +# 1#)
      where
        !ch = indexCharOffAddr# addr nh

foldr# ∷ S → (Char  → a → a) → a → a

-- Usually the unpack-list rule turns foldr# into unpack#

-- It also has a BuiltInRule in PrelRules.hs:
--      foldr# "foo" c (foldr# "baz" c n)
--        =  foldr# "foobaz" c n

{-# NOINLINE foldr# #-}
-- At one stage I had NOINLINE [0] on the grounds that, unlike
-- unpack#, there *is* some point in inlining
-- foldr#, because we get better code for the
-- higher-order function call.  BUT there may be a lot of
-- literal strings, and making a separate 'unpack' loop for
-- each is highly gratuitous.  See nofib/real/anna/PrettyPrint.

foldr# addr f z
  = unpack 0#
  where
    unpack nh
      | isTrue# (ch `eqChar#` '\0'#) = z
      | True                         = C# ch `f` unpack (nh +# 1#)
      where
        !ch = indexCharOffAddr# addr nh

-- There's really no point in inlining this for the same reasons as
-- unpack. See Note [Inlining unpack#] above for details.
unpackN# ∷ S → Int# → String.List
{-# NOINLINE unpackN# #-}
unpackN# _addr 0#   = []
unpackN#  addr len# = unpack [] (len# -# 1#)
    where
     unpack acc i#
      | isTrue# (i# <# 0#)  = acc
      | True                =
         case indexCharOffAddr# addr i# of
            ch → unpack (C# ch : acc) (i# -# 1#)
