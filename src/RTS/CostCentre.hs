{-# language DerivingVia,TypeApplications #-}
module RTS.CostCentre (CString,CostCentre(CC#,CC),label,module_,srcSpan) where
import I 
import A
import Bytes
import A.P

newtype CostCentre ∷ T_P where CC# ∷ P → CostCentre
deriving via (P) instance (CostCentre ∈ P)
type CString = P

label,module_,srcSpan ∷ CostCentre → CString
label (CC# p) = index# p 8#
module_ (CC# p) = index# p 16#
srcSpan (CC# p) = index# p 24#
pattern CC ∷ CString -- ^ Label
           → CString -- ^ module
           → CString -- ^ Source Span
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
        lbl <- GHC.peekCString utf8 =<< ccLabel cc
        mdl <- GHC.peekCString utf8 =<< ccModule cc
        loc <- GHC.peekCString utf8 =<< ccSrcSpan cc
        parent <- ccsParent ccs
        if (mdl == "MAIN" && lbl == "MAIN")
           then return acc
           else go parent ((mdl ++ '.':lbl ++ ' ':'(':loc ++ ")") : acc)
-}
