-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.
module Network.Wai.Route.Tree
    ( Tree
    , fromList
    , lookup
    ) where

import Data.List (foldl')
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

import qualified Data.HashMap.Strict  as M
import qualified Data.Text            as T

data Tree a = Tree
    { subtree :: HashMap Text (Tree a)
    , capture :: Maybe (Tree a)
    , payload :: Maybe (a, [Text])
    }

emptyTree :: Tree m
emptyTree = Tree M.empty Nothing Nothing

fromList :: [(Text, a)] -> Tree a
fromList = foldl' addRoute emptyTree
  where
    branch    = fromMaybe emptyTree
    parsePath = filter (not . T.null) . T.split (=='/')
    addRoute t (p,h) = go t (parsePath p) []
      where
        go n [] cs = n { payload = Just (h, cs) }
        go n (c:ps) cs | T.head c == ':' =
            let b = branch $ capture n
            in n { capture = Just (go b ps (T.tail c:cs)) }
        go n (d:ps) cs =
            let b = branch $ M.lookup d (subtree n)
            in n { subtree = M.insert d (go b ps cs) (subtree n) }

lookup :: Tree a -> [Text] -> Maybe (a, [(Text, Text)])
lookup t p = go p [] t
  where
    go []     cvs n = let f (h, cs) = (h, cs `zip` cvs)
                      in f `fmap` payload n
    go (s:ss) cvs n = maybe (capture n >>= go ss (s:cvs))
                            (go ss cvs)
                            (M.lookup s $ subtree n)
