module Network.Wai.Route
    ( Handler
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
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T

type Handler = [(Text, Text)] -> Application

data RouteTree = RouteTree
    { dirs    :: HashMap Text RouteTree
    , capture :: Maybe RouteTree
    , handler :: Maybe (Handler, [Text])
    }

data Seg = Dir     !Text
         | Capture !Text

emptyTree :: RouteTree
emptyTree = RouteTree M.empty Nothing Nothing

route :: [(Text, Handler)] -> Application
route rs rq = maybe notFound ($ rq) $ lookup tree (pathInfo rq)
  where
    tree = mkTree rs
    notFound = return $ responseLBS status404 [] L.empty

mkTree :: [(Text, Handler)] -> RouteTree
mkTree = foldl' addRoute emptyTree
  where
    addRoute t (p,h) = go t (parsePath p) []
      where
        go n [] cs = n { handler = Just (h, reverse cs) }
        go n ((Dir d):ps) cs =
            let b = branch (M.lookup d $ dirs n)
            in n { dirs = M.insert d (go b ps cs) (dirs n) }
        go n ((Capture c):ps) cs =
            let b = branch (capture n)
            in n { capture = Just (go b ps (c:cs)) }
    branch = fromMaybe emptyTree

parsePath :: Text -> [Seg]
parsePath = map f . filter (not . T.null) . T.split (=='/')
  where
    f s | T.head s == ':' = Capture (T.tail s)
        | otherwise       = Dir s

lookup :: RouteTree -> [Text] -> Maybe Application
lookup t p = go p [] t
  where
    mkApp cvs (h, cs) = h $ cs `zip` reverse cvs
    go []     cvs n = mkApp cvs `fmap` handler n
    go (p:ps) cvs n = maybe (capture n >>= go ps (p:cvs))
                            (go ps cvs)
                            (M.lookup p $ dirs n)
