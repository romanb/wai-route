-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Network.Wai.Route.Tree
    ( -- * Routing Tree
      Tree
    , fromList
    , lookup
    , foldTree
    , mapTree
    , toList
    , segments

      -- ** Tree leaf payload
    , Payload
    , value
    , path
    , captures

      -- ** Captures
    , Captures
    , captured
    , captureParams
    , captureValues
    ) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.List (foldl')
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Data.Semigroup
import Data.Word
import Network.HTTP.Types (urlDecode, urlEncode)
import Prelude hiding (lookup)

import qualified Data.ByteString     as B
import qualified Data.HashMap.Strict as M

data Tree a = Tree
    { subtree :: HashMap ByteString (Tree a)
    , capture :: Maybe (Tree a)
    , payload :: Maybe (Payload a)
    } deriving (Eq, Show)

data Payload a = Payload
    { path     :: !ByteString
    , value    :: !a
    , captures :: !Captures
    } deriving (Eq, Show)

data Captures = Captures
    { params :: [ByteString]
    , values :: [ByteString]
    } deriving (Eq, Show)

instance Semigroup (Tree a) where
    a <> b =  Tree (subtree a <> subtree b)
                   (capture a <> capture b)
                   (payload a <|> payload b)

instance Monoid (Tree a) where
    mempty  = Tree mempty Nothing Nothing
    mappend = (<>)

captureParams :: Captures -> [ByteString]
captureParams = params

captureValues :: Captures -> [ByteString]
captureValues = values

captured :: Captures -> [(ByteString, ByteString)]
captured (Captures a b) = zip a b

fromList :: [(ByteString, a)] -> Tree a
fromList = foldl' addRoute mempty
  where
    addRoute t (p,a) = go t (segments p) []
      where
        go n [] cs =
            let pa = Payload p a (Captures cs [])
            in n { payload = Just pa }

        go n (c:ps) cs | B.head c == colon =
            let b = fromMaybe mempty $ capture n
            in n { capture = Just $! go b ps (B.tail c : cs) }

        go n (d:ps) cs =
            let d' = urlEncode False d
                b  = fromMaybe mempty $ M.lookup d' (subtree n)
            in n { subtree = M.insert d' (go b ps cs) (subtree n) }

lookup :: Tree a -> [ByteString] -> Maybe (Payload a)
lookup t p = go p [] t
  where
    go [] cvs n =
        let f e = e { captures = Captures (params (captures e)) cvs }
        in f <$> payload n

    go (s:ss) cvs n =
        maybe (capture n >>= go ss (urlDecode False s : cvs))
              (go ss cvs)
              (M.lookup s $ subtree n)

foldTree :: (Payload a -> b -> b) -> b -> Tree a -> b
foldTree f z (Tree sub cap pay) =
    let a = M.foldl' (foldTree f) z sub
        b = maybe a (foldTree f a) cap
        c = maybe b (flip f b) pay
    in c

mapTree :: (Payload a -> Payload b) -> Tree a -> Tree b
mapTree f t = foldTree apply mempty t
  where
    apply x tr = tr { payload = Just (f x) }

toList :: Tree a -> [Payload a]
toList = foldTree (:) []

segments :: ByteString -> [ByteString]
segments = filter (not . B.null) . B.split slash

slash, colon :: Word8
slash = 0x2F
colon = 0x3A
{-# INLINE slash #-}
{-# INLINE colon #-}

