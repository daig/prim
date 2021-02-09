--------------------------------------------------------------------
-- | Description : Cost Center Annotations for profiling and stack traces
--------------------------------------------------------------------
-- Running a ghc program with the @-p@ RTS option generates a file @prog.prof@
-- with profiling information using the information from cost centres, but
-- the can be accessed manually inside a Haskell program with
-- 'RTS.CC.Stack.get' and 'RTS.CC.Stack.getCurrent'
--
-- See more [here](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/profiling.html#cost-centres-and-cost-centre-stacks)
{-# language DerivingVia,TypeApplications #-}
module RTS.CC (CC(CC#,CC),label,module_,srcSpan) where
import Prim.I 
import Prim.A
import Prim.A.P
import String.C
import Prim.A.Prim.Elts

-- | A cost center used for profiling and stack traces.
-- Insert manually with an @SCC@ ("set cost centre") annotation:
--
-- @{-# SCC "myLabel" #-} <expression>@
--
-- or 
--
-- @{-# SCC myIdentifier #-} <expression>@
--
-- if @myIdentifier@ is a camelcase Haskell identifier.
--
-- Alternatively, running ghc with @-fprof-auto@ inserts a cost centre
-- automatically around every binding not marked INLINE.
newtype CC ∷ T_P where CC# ∷ P# → CC

label,module_,srcSpan ∷ CC → S
-- | A descriptive string roughly identifying the cost-centre.
-- Set in the annotation @{-# SCC "label" #-}@
label (CC# p) = indexP# p 8#
-- | The module the annotation occurs in
module_ (CC# p) = indexP# p 16#
-- | a
srcSpan (CC# p) = indexP# p 24#
pattern CC ∷ S -- ^ Label
           → S -- ^ module
           → S -- ^ Source Span
           → CC
pattern CC l m s ← (\p → (# label p, module_ p, srcSpan p #) → (# l,m,s #))
{-# complete CC #-}

--label p = index# 
{-
-- | Format a 'CCStack' as a list of lines.
ccsToStrings :: Stack -> IO [[Char]]
ccsToStrings ccs0 = go ccs0 []
  where
    go ccs acc = case css of
       Null → η acc
       _ → do

        cc  <- ccsCC ccs
        lbl <- GHC.peekS utf8 =<< ccLabel cc
        mdl <- GHC.peekS utf8 =<< ccModule cc
        loc <- GHC.peekS utf8 =<< ccSrcSpan cc
        parent <- ccsParent ccs
        if (mdl == "MAIN" && lbl == "MAIN")
           then return acc
           else go parent ((mdl ++ '.':lbl ++ ' ':'(':loc ++ ")") : acc)
-}
