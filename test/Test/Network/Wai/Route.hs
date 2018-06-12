-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Test.Network.Wai.Route (tests) where

import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import Data.Semigroup
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Internal (ResponseReceived (..))
import Network.Wai.Route
import Network.Wai.Route.Tree (Tree)
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.ByteString.Char8  as C
import qualified Network.Wai.Route.Tree as Tree

tests :: TestTree
tests = testGroup "Network.Wai.Route"
    [ testProperty "route" checkRouting
    , testGroup "Network.Wai.Route.Tree"
        [ testProperty "identity" checkMonoidId
        , testProperty "associativity" checkMonoidAssoc
        ]
    ]

checkRouting :: Property
checkRouting = forAll genRoutes check
  where
    check routes =
        let f = route $ map (fmap unHandler) routes
            k = const $ return ResponseReceived
        in conjoin . flip map routes $ \(p, h) ->
            forAll (genReq p) $ \(params, rq) ->
                let s = execState (f rq k) (-1, [])
                in s == (handlerId h, params)

checkMonoidId :: Property
checkMonoidId = forAll genTree $ \t ->
    t <> mempty == t && mempty <> t == t

checkMonoidAssoc :: Property
checkMonoidAssoc = forAll (replicateM 3 genTree) $ \[t1,t2,t3] ->
    (t1 <> t2) <> t3 == t1 <> (t2 <> t3)

-------------------------------------------------------------------------------
-- Generators & helpers

type Params = [(ByteString, ByteString)]

data TestHandler = TestHandler
    { handlerId :: !Int
    , unHandler :: Handler (State (Int, Params))
    }

instance Eq TestHandler where
    h1 == h2 = handlerId h1 == handlerId h2

instance Show TestHandler where
    show h = "<test-handler-" ++ show (handlerId h) ++ ">"

handler :: Int -> TestHandler
handler i = TestHandler i $ \p _ k -> do
    put (i, p)
    k $ responseLBS status200 [] mempty

-- Generate a name for a capture segment in a route.
genCapture :: Gen ByteString
genCapture = (":"<>) . C.pack <$> listOf1 arbitraryASCIIChar `suchThat` f
  where
    f d = '/' `notElem` d

-- Generate a name for a static segment in a route. These are disambiguated from
-- generated parameter values by a fixed prefix, namely '_'. See 'genParam'.
genDir :: Gen ByteString
genDir = C.pack . ('_':) <$> listOf1 arbitraryASCIIChar `suchThat` f
  where
    f d = head d /= ':' && '/' `notElem` d

-- Generate a parameter value for a capture segment to be used in a request.
-- These are disambiguated from static segments by a fixed prefix, namely 'p',
-- in order not to generate values that accidentily match a static segment of
-- some other route.
genParam :: Gen ByteString
genParam = C.pack . ('p':) <$> listOf1 arbitraryASCIIChar

genRoute :: ByteString -> Gen ByteString
genRoute prefix = do
    n <- choose (1, 10)
    s <- vectorOf n (oneof [genDir, genCapture])
    return $ prefix <> C.intercalate "/" s

-- Generates routing tables without ambiguous routes (by giving every route a
-- prefix that is unique in this routing table).
genRoutes :: Gen [(ByteString, TestHandler)]
genRoutes = do
    n <- choose (0, 100)
    r <- mapM (genRoute . C.pack . show) [1..n]
    return $ r `zip` map handler [1..n]

genTree :: Gen (Tree TestHandler)
genTree = Tree.fromList <$> genRoutes

-- Generate a request with a path matching the given route,
-- replacing captures with actual parameter values.
genReq :: ByteString -> Gen (Params, Request)
genReq r = do
    -- Pair each segment of the route with a potential parameter value.
    -- Those values which are paired with a capture are the actual parameters.
    let segs = C.split '/' r
    values <- vectorOf (length segs) genParam
    let segs' = segs `zip` values
    return (mkParams segs', mkReq segs')
  where
    mkReq segs = defaultRequest
          { rawPathInfo = C.intercalate "/" (map mkSeg segs)
          }

    mkSeg (seg, val)
        | C.head seg == ':' = urlEncode False val
        | otherwise         = urlEncode False seg

    mkParams = reverse . foldr go []
      where
        go (seg, val) params
            | C.head seg == ':' = (C.tail seg, val) : params
            | otherwise         = params

