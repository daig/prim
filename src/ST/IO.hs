{-# language NoImplicitPrelude #-}
module ST.IO where
import GHC.Prim (RealWorld,TYPE)
import ST (ST,ST_)


type (☸) = RealWorld
type IO (a :: TYPE r) = ST (☸) a
type IO_ = ST_ (☸)
