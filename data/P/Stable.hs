--------------------------------------------------------------------
-- | Description : Refs with stable address, safe to pass to FFI
--------------------------------------------------------------------
module P.Stable where

new ∷ a → IO (P_Stable a)
new = makeStablePtr#

read ∷ P_Stable a → IO a
read = deRefStablePtr#
