--------------------------------------------------------------------
-- | Description : Primitive Mutable Array types
--------------------------------------------------------------------
{-# language TypeFamilyDependencies, FlexibleInstances,InstanceSigs,MultiParamTypeClasses #-}
{-# language CPP #-}
{-# language QuantifiedConstraints #-}
{-# language ForeignFunctionInterface, UnliftedFFITypes #-}
module Prim.A.M where
import P hiding (Prim)
import Prim.Char
import Prim.I32 (I32(..))
import Prim.I16 (I16(..))
import Prim.I8 (I8(..))
import Prim.I64 (I64(..))
import Prim.I
import qualified P.Stable as Stable
import {-# source #-} qualified Prim.A.Prim as Prim
import {-# source #-} qualified Prim.A.Prim.Pinned as Pinned
import {-# source #-} qualified Prim.A.Array as Ref
import {-# source #-} qualified Prim.A.Boxed.Big as Big
import {-# source #-} qualified Prim.A.Boxed as Boxed
import qualified P.STM as STM
import Stock.Int
#include "MachDeps.h"
#include "HsBaseConfig.h"
import GHC.Types (IO(..))

type family M (a ∷ k) (s ∷ T) = (ma ∷ k) | ma → a where
  M Bytes s = MBytes s
  M Refs  s = MRefs s
  M (P (x ∷ T_ r)) s = P x
  M (Prim.A (x ∷ T_ r_prim)) s = Prim.MA s x
  M (Pinned.A (x ∷ T_ r_prim)) s = Pinned.MA s x
  M (Boxed.A (x ∷ T)) s = Boxed.MA s x
  M (Big.A (x_big ∷ T)) s = Big.MA s x_big
  M (Ref.A (x ∷ T_A)) s = Ref.MA s x
