{-# language PatternSynonyms #-}
module Proxy where

-- | Hack to expose 'proxy#'
pattern Proxy :: Proxy a
pattern Proxy <- proxy# where Proxy = proxy#
