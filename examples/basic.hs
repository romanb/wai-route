{-# LANGUAGE
    TypeApplications
  , DataKinds
  , GADTs
  , OverloadedStrings
  , LambdaCase
#-}

module Examples.Basic where

import Data.Binary.Builder (toLazyByteString)
import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Internal
import Network.Wai.Route
import Prelude

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text as Text

-- $setup
-- >>> :set -XOverloadedStrings

-- Routes

routes :: [Route IO]
routes =
    [ defRoute (str "add" ./ var @"x" ./ var @"y" ./ end)
        addHandler

    , defRoute (str "echo" ./ some @Text ./ end)
        echoHandler

    , defRoute (str "reverse" ./ some @Text ./ end) $
        \(t ::: Nil) _rq send ->
            let t' = Text.reverse t
            in send (response200 (encodeUtf8 (fromStrict t')))

    , defRoute (str "upper" ./ some @Text ./ end) $
        \(t ::: Nil) -> byMethod $ \case
            GET -> \_rq send ->
                let t' = Text.toUpper t
                in send (response200 (encodeUtf8 (fromStrict t')))
            _ -> app405
    ]

addHandler :: Params '[Var "x" Int, Var "y" Int] -> App IO
addHandler (x ::: y ::: Nil) _rq send =
    send (response200 (LC8.pack (show (x + y))))

echoHandler :: Params '[Some Text] -> App IO
echoHandler (t ::: Nil) =
    withQuery (qdef @"repeat" @Int 1) appQueryError $
        \(rep ::: Nil) _rq send ->
            let t' = Text.replicate rep t
            in send (response200 (encodeUtf8 (fromStrict t')))

response200 :: LC8.ByteString -> Response
response200 = responseLBS status200 []

-- Fake requests

addReq :: Request
addReq = defaultRequest { pathInfo = ["add","1","2"] }

echoReq :: Request
echoReq = defaultRequest { pathInfo = ["echo","Hello"] }

reverseReq :: Request
reverseReq = defaultRequest { pathInfo = ["reverse","Hello"] }

upperReq :: Request
upperReq = defaultRequest { pathInfo = ["upper","Hello"] }

-- Fake response sending

printResponse :: Response -> IO ResponseReceived
printResponse rs = do
    case rs of
        ResponseBuilder s _ b -> do
            putStr (show (statusCode s))
            putStr " "
            putStr (C8.unpack (statusMessage s))
            putStr ": "
            putStrLn (LC8.unpack (toLazyByteString b))
        _ -> return ()
    return ResponseReceived

-- Application

app :: App IO
app = route (compileRoutes routes) app404

-- $examples
--
-- >>> app addReq printResponse
-- 200 OK: 3
--
-- >>> app echoReq printResponse
-- 200 OK: Hello
--
-- >>> app echoReq { queryString = [("repeat", Just "2")] } printResponse
-- 200 OK: HelloHello
--
-- >>> app echoReq { queryString = [("repeat", Just "a")] } printResponse
-- 400 Bad Request: Invalid parameter [repeat=a]...
--
-- >>> app reverseReq printResponse
-- 200 OK: olleH
--
-- >>> app upperReq printResponse
-- 200 OK: HELLO
--
-- >>> app upperReq { requestMethod = "POST" } printResponse
-- 405 Method Not Allowed:

