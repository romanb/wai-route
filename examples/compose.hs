{-# LANGUAGE
    TypeApplications
  , DataKinds
  , GADTs
  , OverloadedStrings
#-}

module Examples.Wildcard where

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

prefixRoutes :: [Route IO]
prefixRoutes = [ defRoute (str "files" ./ end) filesHandler ]

otherRoutes :: [Route IO]
otherRoutes = [ defRoute (str "echo" ./ some @Text ./ end) echoHandler ]

filesHandler :: Params '[] -> App IO
filesHandler Nil rq send =
    let path = Text.intercalate "/" (pathInfo rq)
    in send (response200 (encodeUtf8 (fromStrict path)))

echoHandler :: Params '[Some Text] -> App IO
echoHandler (t ::: Nil) _rq send =
    send (response200 (encodeUtf8 (fromStrict t)))

response200 :: LC8.ByteString -> Response
response200 = responseLBS status200 []

-- Fake requests

filesReq :: Request
filesReq = defaultRequest { pathInfo = ["files","foo","bar","baz.jpg"] }

echoReq :: Request
echoReq = defaultRequest { pathInfo = ["echo","Hello"] }

bogusReq :: Request
bogusReq = defaultRequest { pathInfo = ["bogus"] }

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
app = routePrefix (compileRoutes prefixRoutes)
    $ route (compileRoutes otherRoutes)
    $ app404

-- $examples
--
-- >>> app filesReq printResponse
-- 200 OK: foo/bar/baz.jpg
--
-- >>> app echoReq printResponse
-- 200 OK: Hello
--
-- >>> app bogusReq printResponse
-- 404 Not Found:

