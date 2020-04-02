{-# language NoImplicitPrelude #-}
module ST ( ST, ST_) where
import GHC.Prim (State#, TYPE )

type Token = State#
type ST_ s = Token s -> Token s
type ST s (a :: TYPE r) = Token s -> (# Token s, a #)
