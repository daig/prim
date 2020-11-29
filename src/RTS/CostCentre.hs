{-# language DerivingVia,TypeApplications #-}
module RTS.CostCentre (CostCentre(CC#,CC),label,module_,srcSpan) where
import I 
import A
import A.P
import String.C

newtype CostCentre ∷ T_P where CC# ∷ P# → CostCentre

label,module_,srcSpan ∷ CostCentre → S
label (CC# p) = indexP# p 8#
module_ (CC# p) = indexP# p 16#
srcSpan (CC# p) = indexP# p 24#
pattern CC ∷ S -- ^ Label
           → S -- ^ module
           → S -- ^ Source Span
           → CostCentre
pattern CC l m s ← (\p → (# label p, module_ p, srcSpan p #) → (# l,m,s #))
{-# complete CC #-}

--label p = index# 
{-
-- | Format a 'CostCentreStack' as a list of lines.
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
