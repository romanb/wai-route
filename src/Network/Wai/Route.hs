-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.
module Network.Wai.Route
    ( Handler
    , route
    ) where

import Data.Text (Text)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Route.Tree (fromList, lookup)
import Prelude hiding (lookup)

import qualified Data.ByteString.Lazy as L
import qualified Data.Text            as T

type Handler m = [(Text, Text)] -- ^ The captured path parameters.
               -> Request       -- ^ The matched 'Request'.
               -> m Response

-- | Routes requests to 'Handler's according to a routing table.
route :: Monad m => [(Text, Handler m)] -> Request -> m Response
route rs rq = case lookup tree path of
    Just (f, c) -> f c rq
    Nothing     -> notFound
  where
    tree = fromList rs
    path = filter (not . T.null) (pathInfo rq)
    notFound = return $ responseLBS status404 [] L.empty
