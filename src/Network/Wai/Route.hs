-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.
module Network.Wai.Route
    ( Handler
    , PathParams
    , App
    , route
    ) where

import Data.List (foldl')
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Network.HTTP.Types
import Network.Wai
import Prelude hiding (lookup)

import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict  as M
import qualified Data.Text            as T

-------------------------------------------------------------------------------
-- Routing

-- | Captured path parameters.
type PathParams = [(Text, Text)]

-- | A generalized 'Application'.
type App m = Request -> m Response

-- | A route 'Handler', given captured 'PathParams', yields an 'App'lication.
type Handler m = PathParams -> App m

-- | Routes requests to 'Handler's according to a routing table.
route :: Monad m => [(Text, Handler m)] -> App m
route rs rq = maybe notFound ($ rq) $ lookup tree path
  where
    tree = mkTree rs
    path = filter (not . T.null) (pathInfo rq)
    notFound = return $ responseLBS status404 [] L.empty

lookup :: Monad m => Node m -> [Text] -> Maybe (App m)
lookup t p = go p [] t
  where
    go []     cvs n = let f (h, cs) = h (cs `zip` cvs)
                      in f `fmap` handler n
    go (p:ps) cvs n = maybe (capture n >>= go ps (p:cvs))
                            (go ps cvs)
                            (M.lookup p $ dirs n)

-------------------------------------------------------------------------------
-- Tree construction

data Node m = Node
    { dirs    :: HashMap Text (Node m)
    , capture :: Maybe (Node m)
    , handler :: Maybe (Handler m, [Text])
    }

emptyTree :: Node m
emptyTree = Node M.empty Nothing Nothing

mkTree :: Monad m => [(Text, Handler m)] -> Node m
mkTree = foldl' addRoute emptyTree
  where
    branch    = fromMaybe emptyTree
    parsePath = filter (not . T.null) . T.split (=='/')
    addRoute t (p,h) = go t (parsePath p) []
      where
        go n [] cs = n { handler = Just (h, cs) }
        go n (c:ps) cs | T.head c == ':' =
            let b = branch $ capture n
            in n { capture = Just (go b ps (T.tail c:cs)) }
        go n (d:ps) cs =
            let b = branch $ M.lookup d (dirs n)
            in n { dirs = M.insert d (go b ps cs) (dirs n) }
