module F4 (F4
            -- * Instances
            , module X
            -- * misc utilities
            , decode
            ) where
import Num as X

decode ∷ F4 → (# I4 , I4 #)
decode x = case decodeFloat_Int# x of (# cast → m, cast → e #) → (# m, e #)
