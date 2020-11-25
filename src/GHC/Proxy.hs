{-# language PatternSynonyms #-}
module GHC.Proxy where

type Proxy = Proxy#

-- | Hack to expose 'proxy#'
pattern Proxy ∷ Proxy a
pattern Proxy <- proxy# where Proxy = proxy#
