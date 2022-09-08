module P.Stable where

new ∷ a → IO (P_Stable a)
new = makeStablePtr#

read ∷ P_Stable a → IO a
read = deRefStablePtr#
