module CostCentre where

type Stack = Addr#

-- | Get the 'CostCentreStack associated with a given value
get ∷ a → ST s CostCentreStack
get = getCCSOf#
-- | Get the current CostCentreStack (or null if not compiled with profiling).
-- Takes a dummy argument to avoid being floated out by the simplifier,
-- which would result in an uninformative stack ("CAF").
getCurrent ∷ dummy → ST s CostCentreStack
getCurrent = getCurrentCCS#

-- | Run a compuation with an empty cost-centre stack. For example, this is
-- used by the interpreter to run an interpreted computation without the call
-- stack showing that it was invoked from GHC.
clear ∷ ST s a → ST s a
clear = clearCCS#
