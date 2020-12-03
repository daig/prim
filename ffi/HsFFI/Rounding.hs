--------------------------------------------------------------------
-- | Description : Rounding mode type for floating arithmetic
--------------------------------------------------------------------
{-# language ForeignFunctionInterface, CApiFFI, UnliftedFFITypes, GHCForeignImportPrim #-}
module HsFFI.Rounding (Rounding(Rounding#,Indeterminable,Zero,Nearest,Inf,Inf_)) where

-- | Rounding mode used by floating arithmetic
newtype Rounding = Rounding# Int
pattern Indeterminable = Rounding# (-1)
pattern Zero = Rounding# 0
pattern Nearest = Rounding# 1
pattern Inf = Rounding# 2
-- | Round towards  -infinity
pattern Inf_ = Rounding# 2
