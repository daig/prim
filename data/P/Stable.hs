--------------------------------------------------------------------
-- | Description : Refs with stable address, safe to pass to FFI
--------------------------------------------------------------------
module P.Stable where

new ∷ a → IO (Stable# a)
new = makeStablePtr#

read ∷ Stable# a → IO a
read = deRefStablePtr#
