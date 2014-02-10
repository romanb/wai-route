-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Network.Wai.Route.Tree
    ( Tree
    , fromList
    , lookup
    , segments
    ) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.List (foldl')
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Word
import Network.HTTP.Types (urlDecode, urlEncode)
import Prelude hiding (lookup)

import qualified Data.ByteString     as B
import qualified Data.HashMap.Strict as M

data Tree a = Tree
    { subtree :: HashMap ByteString (Tree a)
    , capture :: Maybe (Tree a)
    , payload :: Maybe (a, [ByteString])
    }

instance Monoid (Tree a) where
    mempty        = Tree mempty Nothing Nothing
    a `mappend` b = Tree (subtree a <> subtree b)
                         (capture a <> capture b)
                         (payload a <|> payload b)

fromList :: [(ByteString, a)] -> Tree a
fromList = foldl' addRoute mempty
  where
    addRoute t (p,pl) = go t (segments p) []
      where
        go n [] cs = n { payload = Just (pl, cs) }
        go n (c:ps) cs | B.head c == colon =
            let b = fromMaybe mempty $ capture n
            in n { capture = Just $! go b ps (B.tail c : cs) }
        go n (d:ps) cs =
            let d' = urlEncode False d
                b  = fromMaybe mempty $ M.lookup d' (subtree n)
            in n { subtree = M.insert d' (go b ps cs) (subtree n) }

lookup :: Tree a -> [ByteString] -> Maybe (a, [(ByteString, ByteString)])
lookup t p = go p [] t
  where
    go []     cvs n = let f (pl, cs) = (pl, cs `zip` cvs)
                      in f `fmap` payload n
    go (s:ss) cvs n = maybe (capture n >>= go ss (urlDecode False s : cvs))
                            (go ss cvs)
                            (M.lookup s $ subtree n)

segments :: ByteString -> [ByteString]
segments = filter (not . B.null) . B.split slash

slash, colon :: Word8
slash = 0x2F
colon = 0x3A
