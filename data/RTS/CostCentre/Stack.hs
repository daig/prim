--------------------------------------------------------------------
-- | Description : Runtime profiling and call-trees
--
-- see <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#cost-centres-and-cost-centre-stacks Cost centres and cost-centre stacks>
--------------------------------------------------------------------
{-# language DerivingVia,TypeApplications #-}
module RTS.CostCentre.Stack
  ( -- * Datatype
  Stack(Stack#)
  -- * Primitive Operations
  , get , getCurrent, clear
  -- * Utility operations
--  , toStrings
  ) where
import RTS.CostCentre

-- | A Cost Centre Stack, represented as a linked list of 'CostCentre'
newtype Stack = Stack# Addr#
deriving newtype instance (≡) Stack

-- | Get the 'Stack' associated with a given value
get ∷ a → ST s Stack
get a = coerce do getCCSOf# a
-- | Get the current 'Stack' (or 'Null' if not compiled with profiling).
-- Takes a dummy argument to avoid being floated out by the simplifier,
-- which would result in an uninformative stack ("CAF").
getCurrent ∷ dummy → ST s Stack
getCurrent a =  coerce do getCurrentCCS# a

-- | Run a compuation with an empty cost-centre stack. For example, this is
-- used by the interpreter to run an interpreted computation without the call
-- stack showing that it was invoked from GHC.
clear ∷ ST s a → ST s a
clear = clearCCS#

{-
-- | Format a 'CCS' as a list of lines.
toStrings ∷ Stack → [[Char]]
toStrings ccs0 = go ccs0 []
  where
    go ∷ Stack → [[Char]] → [[Char]]
    go ccs acc = case ccs of
       Null → acc
       Stack (CostCentre (unpack# → lbl) (unpack# → mdl) (unpack# → loc)) parent
         → if mdl == "MAIN" ∧ lbl == "MAIN" then acc
             else go parent ((mdl ++ '.':lbl ++ ' ':'(':loc ++ ")") : acc)
infixr 5 ++
[] ++ bs = bs
(a : as) ++ bs = a : as ++ bs
-}
