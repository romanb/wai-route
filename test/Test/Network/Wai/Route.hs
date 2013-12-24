-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Network.Wai.Route (tests) where

import Control.Arrow (first)
import Control.Applicative ((<$>))
import Control.Monad.State.Strict
import Data.List (nub)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Route
import Test.QuickCheck (Gen)
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy  as L
import qualified Data.Text             as T

tests :: TestTree
tests = testProperty "Routing" checkRouting

checkRouting :: Property
checkRouting = forAll genRoutes check
  where
    check routes =
        let h1  = route $ map (fmap unHandler) routes
            rsv = routes >>= T.split (=='/') . fst
        in conjoin . flip map routes $ \(r, TestHandler h2) ->
            forAll (genReq r rsv) $ \(params2, rq) ->
                let result1 = h1 rq    -- routed
                    result2 = h2 [] rq -- direct
                    (hId1, params1) = execState result1 (-1, [])
                    (hId2,       _) = execState result2 (-1, [])
                in hId1 == hId2 && params1 == params2

-------------------------------------------------------------------------------
-- Generators & helpers

newtype TestHandler = TestHandler
    { unHandler :: Handler (State (Int, [(Text, Text)])) }

instance Show TestHandler where
    show _ = "<test-handler>"

handler :: Int -> TestHandler
handler i = TestHandler $ \p _ -> do
    put (i, p)
    return $ responseLBS status200 [] L.empty

genDir :: Gen Text
genDir =  decodeUtf8 . urlEncode False . C.pack
      <$> listOf1 arbitrary `suchThat` ((/=':') . head)

genCapture :: Gen Text
genCapture =  (":"<>) . decodeUtf8 . urlEncode False . C.pack
          <$> listOf1 arbitrary

genRoute :: Gen Text
genRoute = do
    n <- choose (1, 10)
    s <- vectorOf n (oneof [genDir, genCapture])
    return $ T.intercalate "/" s

-- Generates random routing tables without ambiguous routes.
-- Two routes are ambiguous if they have an equal number of segments and
-- equal static segments appear in the same positions.
genRoutes :: Gen [(Text, TestHandler)]
genRoutes = do
    n <- choose (0, 10)
    r <- vectorOf n genRoute `suchThat` noAmbiguity
    return $ r `zip` map handler [0..n]
  where
    noAmbiguity rs = let rs' = map normalize rs
                     in length (nub rs') == length rs'
    normalize = filter ((/=':') . T.head . snd)
              . zip ([0..] :: [Int])
              . T.split (=='/')

-- Generate a request with a path matching the given route.
genReq :: Text   -- ^ Route
       -> [Text] -- ^ Reserved names
       -> Gen ([(Text, Text)], Request)
genReq r reserved = do
    values <- vectorOf (length segs) genDir `suchThat` all (`notElem` reserved)
    let zipped = segs `zip` values
        params = map (first T.tail) . filter ((==':') . T.head . fst) $ zipped
        rq = defaultRequest { pathInfo = map toSeg zipped }
    return (reverse params, rq)
  where
    segs = T.split (=='/') r
    toSeg (s, v) | T.head s == ':' = v
                 | otherwise       = s

instance Show Request where
    show = show . T.intercalate "/" . pathInfo
