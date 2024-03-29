--------------------------------------------------------------------
-- | Description : Runtime profiling and call-trees
--
-- see <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#cost-centres-and-cost-centre-stacks Cost centres and cost-centre stacks>
--------------------------------------------------------------------
{-# language DerivingVia,TypeApplications #-}
module RTS.CC.Stack
  ( -- * Datatype
  CCS(CCS#,CCS,Null),head,parent
  -- * Primitive Operations
  , get , getCurrent, clear
  -- * Utility operations
--  , toStrings
  ) where
import Prim.I 
import Prim.A
import Prim.A.Prim.Elts
import Prim.A.P
import qualified RTS.CC as CC
import RTS.CC (CC(..))
import Stock.Char
import Stock.Eq
import Prim.IO
import qualified String.C as S

-- | A Cost Centre Stack, represented as a linked list of 'CC'
newtype CCS ∷ T_P where CCS# ∷ P# → CCS 
deriving newtype instance (≡) CCS

head ∷ CCS → CC
head (CCS# p) = CC# (indexP# p 8#)
parent ∷ CCS → CCS
parent (CCS# p) = CCS# (indexP# p 16#)

-- | An empty @CCS@
pattern Null ← CCS# nullAddr# where Null = CCS# nullAddr#

-- | A Cost
pattern CCS ∷ CC {- ^ Head -} → CCS {- ^ Parent -} → CCS
pattern CCS h p ← ((\p → (# head p , parent p #)) → (# h , p #))
{-# complete Null, CCS #-}

-- | Get the 'CCS' associated with a given value
get ∷ a → ST s CCS
get a = coerce do getCCSOf# a
-- | Get the current 'CCS' (or 'Null' if not compiled with profiling).
-- Takes a dummy argument to avoid being floated out by the simplifier,
-- which would result in an uninformative stack ("CAF").
getCurrent ∷ dummy → ST s CCS
getCurrent a =  coerce do getCurrentCCS# a

-- | Run a compuation with an empty cost-centre stack. For example, this is
-- used by the interpreter to run an interpreted computation without the call
-- stack showing that it was invoked from GHC.
clear ∷ ST s a → ST s a
clear = clearCCS#

{-
-- | Format a 'CSS' as a list of lines.
toStrings ∷ CCS → [[Char]]
toStrings ccs0 = go ccs0 []
  where
    go ∷ CCS → [[Char]] → [[Char]]
    go ccs acc = case ccs of
       Null → acc
       CCS (CC (unpack# → lbl) (unpack# → mdl) (unpack# → loc)) parent
         → if mdl == "MAIN" ∧ lbl == "MAIN" then acc
             else go parent ((mdl ++ '.':lbl ++ ' ':'(':loc ++ ")") : acc)
infixr 5 ++
[] ++ bs = bs
(a : as) ++ bs = a : as ++ bs
-}
