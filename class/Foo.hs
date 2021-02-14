{-# language QualifiedDo, LinearTypes #-}
module Foo where
import qualified Do as ST
import Unsafe.Coerce
import Num

readInt ∷ I ⊸ ST (M A#) I
readInt = λ\ i → λ\ (M_UnpinnedByteArray# m)
  → (# M_UnpinnedByteArray# m , ST.run (λ do readIntArray# `λ` m `λ` i) #)

lift  ∷ ((M A#) ⊸ (M A#)) ⊸ ST (M A#) (##)
lift st_ s = go (st_ s) where
  go ∷ ST (M A#) (##)
  go s' = (# s' , (##) #)

writeInt ∷ I ⊸ I ⊸ ST (M A#) (##)
writeInt = λ\ i → λ\ x → λ\ (M_UnpinnedByteArray# m)
   → let st_ = writeIntArray# m i x
     in runRW# do \s → case st_ s of _ → (# M_UnpinnedByteArray# m , (##) #)
     --(# M_Unpinned# m , run (linear2# (writeIntArray# m i)) #)

foo ∷ I ⊸ ST M_A# (##)
foo i0 = go do δ i0 where
  go ∷ (# I , I #) ⊸ ST M_A# (##)
  go (# i , j #) = ST.do
    writeInt i 3#
    foo (j+1# )
