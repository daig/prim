module Ref.U32 where
import Ref.Byte

index# :: Ref -> I64 {- ^ Offset in elments -} -> U32
index# = indexWord32OffAddr#

read# :: Ref -> I64 {- ^ Offset in elements -} -> ST s U32
read# = readWord32OffAddr#

write# :: Ref -> I64 {- ^ Offset in elements -} -> U32 -> ST_ s
write# = writeWord32OffAddr#
