module IO.Exception where
import Prelude

-- | Catches /all/ types of exceptions by unsafely coercing it to @e@
--
--  Async exceptions are masked automatically during
--  the execution of an exception handler.
catch# ∷ IO a → (e → IO a) → IO a
catch# = Prelude.catch#

-- | Raising values other than type @SomeException@ leads to segfault
raise# ∷ ∀ {r} (a ∷ T r) e. e → a
raise# = Prelude.raise#
-- | Raising values other than type @SomeException@ leads to segfault
raiseIO ∷ e → IO a
raiseIO = raiseIO#

-- | Evaluate the argument to weak head normal form.
-- Always produces a valid 'IO' action which throws on /execution/
-- iff the value 'raise#'s on /evaluation/.
-- In contrast 'seq' will throw an exception.
--
-- The practical implication of this difference is that due to the /imprecise exceptions/ semantics,
--
-- If you are forcing a lazy value for efficiency reasons only and do not
-- care about exceptions, you may use 'seq'
--
-- see https://gitlab.haskell.org/ghc/ghc/-/blob/fc644b1a643128041cfec25db84e417851e28bab/compiler/GHC/Core/Opt/ConstantFold.hs#L1198
eval ∷ a → IO a
eval = seq#
