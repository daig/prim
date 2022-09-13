--------------------------------------------------------------------
-- | Description : Cost Center Annotation type for profiling and stack traces
--------------------------------------------------------------------
-- Running a ghc program with the @-p@ RTS option generates a file @prog.prof@
-- with profiling information using the information from cost centres, but
-- the can be accessed manually inside a Haskell program with
-- 'RTS.CostCentre.Stack.get' and 'RTS.CostCentre.Stack.getCurrent'
--
-- See more [here](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/profiling.html#cost-centres-and-cost-centre-stacks)
{-# language DerivingVia,TypeApplications #-}
module RTS.CostCentre (CostCentre(CostCentre#)) where

-- | A cost center used for profiling and stack traces.
-- Insert manually with an @SCostCentre@ ("set cost centre") annotation:
--
-- @{-# SCostCentre "myLabel" #-} <expression>@
--
-- or 
--
-- @{-# SCostCentre myIdentifier #-} <expression>@
--
-- if @myIdentifier@ is a camelcase Haskell identifier.
--
-- Alternatively, running ghc with @-fprof-auto@ inserts a cost centre
-- automatically around every binding not marked INLINE.
newtype CostCentre = CostCentre# P#
