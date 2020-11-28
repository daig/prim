--------------------------------------------------------------------
-- | Description : Rounding mode type for floating arithmetic
--------------------------------------------------------------------
{-# language ForeignFunctionInterface, CApiFFI, UnliftedFFITypes, GHCForeignImportPrim #-}
module HsFFI.Rounding (Rounding(Rounding#,Indeterminable,Zero,Nearest,Inf,Inf_)) where
import Stock.Int
import Stock.Word
import Stock.Float
import Stock.Double
import Stock.IO

-- | Rounding mode used by floating arithmetic
newtype Rounding = Rounding# Int
pattern Indeterminable = Rounding# (-1)
pattern Zero = Rounding# 0
pattern Nearest = Rounding# 1
pattern Inf = Rounding# 2
-- | Round towards  -infinity
pattern Inf_ = Rounding# 2
