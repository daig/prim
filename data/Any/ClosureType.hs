--------------------------------------------------------------------
-- | Description : Closure Type tags from @Rts.h@
--
-- See <https://downloads.haskell.org/ghc/8.10.2/docs/html/libraries/ghc-heap-8.10.2/index.html ghc-heap>
--------------------------------------------------------------------
{-# language CPP #-}
{-# language ForeignFunctionInterface, CApiFFI, UnliftedFFITypes, GHCForeignImportPrim #-}
module Any.ClosureType (ClosureType(ClosureType#, 
  Invalid, Constr, Constr_1_0, Constr_0_1, Constr_2_0, Constr_1_1, Constr_0_2, Constr_NoCAF, Fun,
  Fun_0_1, Fun_2_0, Fun_1_1, Fun_0_2, Fun_Static, Thunk, Thunk_0_1, Thunk_2_0, Thunk_1_1, Thunk_0_2,
  Thunk_Static, Thunk_Selector, Bco, Ap, Pap, AP_Stack, Ind, Ind_Static, Ret_BCO, Ret_SMALL, Ret_BIG,
  Ret_FUN, Update_Frame, Catch_Frame, Underflow_Frame, Stop_Frame, Blocking_Queue, Blackhole, MVar_Clean,
  MVar_Dirty, TVar, Arr_Words, Mut_Arr_Ptrs_Clean, Mut_Arr_Ptrs_Dirty , Mut_Arr_Ptrs_Frozen_Dirty,
  Mut_Arr_Ptrs_Frozen_Clean, Mut_Var_Clean, Mut_Var_Dirty, Weak, Prim, Mut_Prim, Tso, Stack, Trec_Chunk,
  Atomically_Frame, Catch_Retry_Frame, Catch_STM_Frame, Whitehole, Small_Mut_Arr_Ptrs_Clean, Small_Mut_Arr_Ptrs_Dirty ,
  Small_Mut_Arr_Ptrs_Frozen_Dirty, Small_Mut_Arr_Ptrs_Frozen_Clean , Compact_NFData, NClosure_Types)
  ) where
#include "rts/storage/ClosureTypes.h"

newtype ClosureType = ClosureType# U
pattern Invalid = ClosureType# INVALID_OBJECT##
pattern Constr = ClosureType# CONSTR##
pattern Constr_1_0 = ClosureType# CONSTR_1_0##
pattern Constr_0_1 = ClosureType# CONSTR_0_1##
pattern Constr_2_0 = ClosureType# CONSTR_2_0##
pattern Constr_1_1 = ClosureType# CONSTR_1_1##
pattern Constr_0_2 = ClosureType# CONSTR_0_2##
pattern Constr_NoCAF = ClosureType# CONSTR_NOCAF##
pattern Fun = ClosureType# FUN##
pattern Fun_0_1 = ClosureType# FUN_0_1##
pattern Fun_2_0 = ClosureType# FUN_2_0##
pattern Fun_1_1 = ClosureType# FUN_1_1##
pattern Fun_0_2 = ClosureType# FUN_0_2##
pattern Fun_Static = ClosureType# FUN_STATIC##
pattern Thunk = ClosureType# THUNK##
pattern Thunk_0_1 = ClosureType# THUNK_0_1##
pattern Thunk_2_0 = ClosureType# THUNK_2_0##
pattern Thunk_1_1 = ClosureType# THUNK_1_1##
pattern Thunk_0_2 = ClosureType# THUNK_0_2##
pattern Thunk_Static = ClosureType# THUNK_STATIC##
pattern Thunk_Selector = ClosureType# THUNK_SELECTOR##
pattern Bco = ClosureType# BCO##
pattern Ap = ClosureType# AP##
pattern Pap = ClosureType# PAP##
pattern AP_Stack = ClosureType# AP_STACK##
pattern Ind = ClosureType# IND##
pattern Ind_Static = ClosureType# IND_STATIC##
pattern Ret_BCO = ClosureType# RET_BCO##
pattern Ret_SMALL = ClosureType# RET_SMALL##
pattern Ret_BIG = ClosureType# RET_BIG##
pattern Ret_FUN = ClosureType# RET_FUN##
pattern Update_Frame = ClosureType# UPDATE_FRAME##
pattern Catch_Frame = ClosureType# CATCH_FRAME##
pattern Underflow_Frame = ClosureType# UNDERFLOW_FRAME##
pattern Stop_Frame = ClosureType# STOP_FRAME##
pattern Blocking_Queue = ClosureType# BLOCKING_QUEUE##
pattern Blackhole = ClosureType# BLACKHOLE##
pattern MVar_Clean = ClosureType# MVAR_CLEAN##
pattern MVar_Dirty = ClosureType# MVAR_DIRTY##
pattern TVar = ClosureType# TVAR##
pattern Arr_Words = ClosureType# ARR_WORDS##
pattern Mut_Arr_Ptrs_Clean = ClosureType# MUT_ARR_PTRS_CLEAN##
pattern Mut_Arr_Ptrs_Dirty  = ClosureType# MUT_ARR_PTRS_DIRTY##
pattern Mut_Arr_Ptrs_Frozen_Dirty   = ClosureType# MUT_ARR_PTRS_FROZEN_DIRTY##
pattern Mut_Arr_Ptrs_Frozen_Clean = ClosureType# MUT_ARR_PTRS_FROZEN_CLEAN##
pattern Mut_Var_Clean = ClosureType# MUT_VAR_CLEAN##
pattern Mut_Var_Dirty = ClosureType# MUT_VAR_DIRTY##
pattern Weak = ClosureType# WEAK##
pattern Prim = ClosureType# PRIM##
pattern Mut_Prim = ClosureType# MUT_PRIM##
pattern Tso = ClosureType# TSO##
pattern Stack = ClosureType# STACK##
pattern Trec_Chunk = ClosureType# TREC_CHUNK##
pattern Atomically_Frame = ClosureType# ATOMICALLY_FRAME##
pattern Catch_Retry_Frame = ClosureType# CATCH_RETRY_FRAME##
pattern Catch_STM_Frame = ClosureType# CATCH_STM_FRAME##
pattern Whitehole = ClosureType# WHITEHOLE##
pattern Small_Mut_Arr_Ptrs_Clean = ClosureType# SMALL_MUT_ARR_PTRS_CLEAN##
pattern Small_Mut_Arr_Ptrs_Dirty  = ClosureType# SMALL_MUT_ARR_PTRS_DIRTY##
pattern Small_Mut_Arr_Ptrs_Frozen_Dirty = ClosureType# SMALL_MUT_ARR_PTRS_FROZEN_DIRTY##
pattern Small_Mut_Arr_Ptrs_Frozen_Clean  = ClosureType# SMALL_MUT_ARR_PTRS_FROZEN_CLEAN##
pattern Compact_NFData = ClosureType# COMPACT_NFDATA##
pattern NClosure_Types = ClosureType# N_CLOSURE_TYPES##

{-# complete
  Invalid, Constr, Constr_1_0, Constr_0_1, Constr_2_0, Constr_1_1, Constr_0_2, Constr_NoCAF, Fun,
  Fun_0_1, Fun_2_0, Fun_1_1, Fun_0_2, Fun_Static, Thunk, Thunk_0_1, Thunk_2_0, Thunk_1_1, Thunk_0_2,
  Thunk_Static, Thunk_Selector, Bco, Ap, Pap, AP_Stack, Ind, Ind_Static, Ret_BCO, Ret_SMALL, Ret_BIG,
  Ret_FUN, Update_Frame, Catch_Frame, Underflow_Frame, Stop_Frame, Blocking_Queue, Blackhole, MVar_Clean,
  MVar_Dirty, TVar, Arr_Words, Mut_Arr_Ptrs_Clean, Mut_Arr_Ptrs_Dirty , Mut_Arr_Ptrs_Frozen_Dirty,
  Mut_Arr_Ptrs_Frozen_Clean, Mut_Var_Clean, Mut_Var_Dirty, Weak, Prim, Mut_Prim, Tso, Stack, Trec_Chunk,
  Atomically_Frame, Catch_Retry_Frame, Catch_STM_Frame, Whitehole, Small_Mut_Arr_Ptrs_Clean, Small_Mut_Arr_Ptrs_Dirty ,
  Small_Mut_Arr_Ptrs_Frozen_Dirty, Small_Mut_Arr_Ptrs_Frozen_Clean , Compact_NFData, NClosure_Types #-}

{-
pokeItbl :: Ptr StgInfoTable -> StgInfoTable -> IO ()
pokeItbl a0 itbl = do
  ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) a0 (ptrs itbl)
  ((\hsc_ptr -> pokeByteOff hsc_ptr 20)) a0 (nptrs itbl)
  ((\hsc_ptr -> pokeByteOff hsc_ptr 24)) a0 (fromEnum (tipe itbl))
  ((\hsc_ptr -> pokeByteOff hsc_ptr 28)) a0 (srtlen itbl)
  let code_offset = a0 `plusPtr` ((32))
  case code itbl of
    Nothing -> return ()
    Just (Left xs) -> pokeArray code_offset xs
    Just (Right xs) -> pokeArray code_offset xs


-- | Read an InfoTable from the heap into a haskell type.
-- WARNING: This code assumes it is passed a pointer to a "standard" info
-- table. If tables_next_to_code is enabled, it will look 1 byte before the
-- start for the entry field.
peekItbl :: Ptr StgInfoTable -> IO StgInfoTable
peekItbl a0 = do
  let ptr = a0
      entry' = Nothing
  ptrs'   <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
  nptrs'  <- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) ptr
  tipe'   <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
  srtlen' <- ((\hsc_ptr -> peekByteOff hsc_ptr 28)) a0
  return StgInfoTable
    { entry  = entry'
    , ptrs   = ptrs'
    , nptrs  = nptrs'
    , tipe   = toEnum (fromIntegral (tipe' :: HalfWord))
    , srtlen = srtlen'
    , code   = Nothing
    }
-}
