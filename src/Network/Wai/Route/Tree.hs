-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.
module Network.Wai.Route.Tree
    ( Tree
    , fromList
    , lookup
    ) where

import Control.Applicative ((<|>))
import Data.List (foldl')
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Prelude hiding (lookup)

import qualified Data.HashMap.Strict  as M
import qualified Data.Text            as T

data Tree a = Tree
    { subtree :: HashMap Text (Tree a)
    , capture :: Maybe (Tree a)
    , payload :: Maybe (a, [Text])
    }

instance Monoid (Tree a) where
    mempty        = Tree mempty Nothing Nothing
    a `mappend` b = Tree (subtree a <> subtree b)
                         (capture a <> capture b)
                         (payload a <|> payload b)

fromList :: [(Text, a)] -> Tree a
fromList = foldl' addRoute mempty
  where
    branch    = fromMaybe mempty
    parsePath = filter (not . T.null) . T.split (=='/')
    addRoute t (p,pl) = go t (parsePath p) []
      where
        go n [] cs = n { payload = Just (pl, cs) }
        go n (c:ps) cs | T.head c == ':' =
            let b = branch $ capture n
            in n { capture = Just (go b ps (T.tail c:cs)) }
        go n (d:ps) cs =
            let b = branch $ M.lookup d (subtree n)
            in n { subtree = M.insert d (go b ps cs) (subtree n) }

lookup :: Tree a -> [Text] -> Maybe (a, [(Text, Text)])
lookup t p = go p [] t
  where
    go []     cvs n = let f (pl, cs) = (pl, cs `zip` cvs)
                      in f `fmap` payload n
    go (s:ss) cvs n = maybe (capture n >>= go ss (s:cvs))
                            (go ss cvs)
                            (M.lookup s $ subtree n)
