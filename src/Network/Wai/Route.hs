-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Route
    ( Handler
    , route
    ) where

import Data.ByteString (ByteString)
import Network.HTTP.Types
import Network.Wai

import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as L
import qualified Network.Wai.Route.Tree as Tree

type Handler m = [(ByteString, ByteString)] -- ^ The captured path parameters.
               -> Request                   -- ^ The matched 'Request'.
               -> m Response

-- | Routes requests to 'Handler's according to a routing table.
route :: Monad m => [(ByteString, Handler m)] -> Request -> m Response
route rs rq = case Tree.lookup (Tree.fromList rs) path of
    Just (f, c) -> f c rq
    Nothing     -> notFound
  where
    path     = filter (not . B.null) (B.split slash $ rawPathInfo rq)
    notFound = return $ responseLBS status404 [] L.empty
    slash    = 0x2F
