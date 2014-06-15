-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Network.Wai.Route (tests) where

import Control.Arrow (first)
import Control.Applicative ((<$>))
import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import Data.List (nub)
import Data.Monoid ((<>))
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Internal (ResponseReceived (..))
import Network.Wai.Route
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy  as L

tests :: TestTree
tests = testProperty "Routing" checkRouting

checkRouting :: Property
checkRouting = forAll genRoutes check
  where
    check routes =
        let h1  = route $ map (fmap unHandler) routes
            rsv = routes >>= C.split '/' . fst
            res = const $ return ResponseReceived
        in conjoin . flip map routes $ \(r, TestHandler h2) ->
            forAll (genReq r rsv) $ \(params2, rq) ->
                let result1 = h1 rq res    -- routed
                    result2 = h2 [] rq res -- direct
                    (hId1, params1) = execState result1 (-1, [])
                    (hId2,       _) = execState result2 (-1, [])
                in hId1 == hId2 && params1 == params2

-------------------------------------------------------------------------------
-- Generators & helpers

newtype TestHandler = TestHandler
    { unHandler :: Handler (State (Int, [(ByteString, ByteString)])) }

instance Show TestHandler where
    show _ = "<test-handler>"

handler :: Int -> TestHandler
handler i = TestHandler $ \p _ k -> do
    put (i, p)
    k $ responseLBS status200 [] L.empty

genDir :: Gen ByteString
genDir = C.pack <$> listOf1 arbitrary `suchThat` f
  where
    f d = head d /= ':' && '/' `notElem` d

genCapture :: Gen ByteString
genCapture =  (":"<>) . C.pack <$> listOf1 arbitrary `suchThat` notElem '/'

genRoute :: Gen ByteString
genRoute = do
    n <- choose (1, 10)
    s <- vectorOf n (oneof [genDir, genCapture])
    return $ C.intercalate "/" s

-- Generates random routing tables without ambiguous routes.
-- Two routes are ambiguous if they have an equal number of segments and
-- equal static segments appear in the same positions.
genRoutes :: Gen [(ByteString, TestHandler)]
genRoutes = do
    n <- choose (0, 10)
    r <- vectorOf n genRoute `suchThat` noAmbiguity
    return $ r `zip` map handler [0..n]
  where
    noAmbiguity rs = let rs' = map normalize rs
                     in length (nub rs') == length rs'
    normalize = filter ((/=':') . C.head . snd)
              . zip ([0..] :: [Int])
              . C.split '/'

-- Generate a request with a path matching the given route.
genReq :: ByteString   -- ^ Route
       -> [ByteString] -- ^ Reserved names
       -> Gen ([(ByteString, ByteString)], Request)
genReq r reserved = do
    values <- vectorOf (length segs) genDir `suchThat` all (`notElem` reserved)
    let zipped = segs `zip` values
        params = reverse . map (first C.tail) . filter ((==':') . C.head . fst) $ zipped
        rq = defaultRequest { rawPathInfo = C.intercalate "/"  $ map toSeg zipped }
    return (params, rq)
  where
    segs = C.split '/' r
    toSeg (s, v) | C.head s == ':' = urlEncode False v
                 | otherwise       = urlEncode False s

instance Show Request where
    show = show . rawPathInfo
