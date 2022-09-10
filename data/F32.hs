module F32 (F32
            -- * Instances
            , module X
            -- * misc utilities
            , decode
            ) where
import Num as X

decode ∷ F32 → (# I {- 32 bit -}, I {- 32 bit -} #)
decode = coerce decodeFloat_Int#
