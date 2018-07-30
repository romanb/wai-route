-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE
    DataKinds
  , GADTs
  , MagicHash
  , OverloadedStrings
  , ScopedTypeVariables
  , StandaloneDeriving
  , StrictData
  , TypeApplications
  , TypeInType
  , TypeOperators
#-}

-- | This module provides a WAI 'Middleware' for routing requests
-- to handlers (i.e. 'Application's) based on the request path,
-- thereby optionally capturing variables.
--
-- The basic functionality is provided by 'route' and 'routePrefix'.
-- The 'RoutingTrie's for the middleware can be constructed directly
-- or by compiling 'Route's, which offers enhanced type-safety.
--
-- Some additional utilities for processing the request method,
-- query parameters and headers in a handler are also provided.
module Network.Wai.Route
    ( -- | The following extensions are used for all inline examples.
      -- $setup

      -- | More extensive examples can be found in the @examples@
      -- directory of the source distribution.

      -- * Routing Middleware
      App
    , Handler
    , RoutingTrie
    , route
    , routePrefix

      -- * Typed Routes
    , Route (..)
    , defRoute
    , compileRoute
    , compileRoutes

      -- ** Paths
    , Path, Vars, Var, Some
    , (=~=)
    , str, var, some, (./), end
    , Params (..)
    , InvalidParam (..)
    , SomePath (..)

      -- ** Low-level
    , parseParams
    , ParseError (..)
    , pathPattern

      -- * Utilities

      -- ** HTTP Methods
    , getStdMethod
    , byStdMethod

      -- ** HTTP Query Parameters
    , getQueryParam
    , getQueryParam'

      -- ** HTTP Headers
    , getHeader
    , InvalidHeader (..)

      -- ** Standard Responses
    , app404
    , app405

      -- * Re-exports
    , Trie
    , Pattern
    , Matcher (..)
    , Capture (..)
    , FromHttpApiData
    ) where

import GHC.Exts (Proxy#, proxy#)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal')

import Data.ByteString (ByteString)
import Data.Kind
import Data.Sequence (Seq (..), (<|))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Builder (fromString, fromText, toLazyText)
import Data.Trie.Pattern
import Network.HTTP.Types
import Network.Wai
import Prelude
import Web.HttpApiData (FromHttpApiData (..))

import qualified Data.ByteString.Char8   as C8
import qualified Data.Sequence           as Seq
import qualified Data.Text               as Text
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Trie.Pattern       as Trie

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications

-- | An 'App' is a WAI 'Application' generalised to any type
-- of kind @* -> *@, and thus in particular any monad, i.e.
-- @App IO ~ Application@.
type App m
    = Request
    -> (Response -> m ResponseReceived)
    -> m ResponseReceived

-- | A handler function for a routed request.
type Handler m
    = Seq (Capture Text)
    -> App m

-- | A routing trie for request paths.
type RoutingTrie m = Trie Text (Handler m)

-- | Routes requests to 'Handler's via a routing trie, passing
-- along captured path parameters. The request path must fully match a route in
-- order for the associated handler function to be called. If no route matches
-- the request path, the request is forwarded to the 'App'lication given as
-- second argument.
route :: Monad m => RoutingTrie m -> App m -> App m
route rt app rq k = case Trie.match (pathInfo rq) rt of
    Just (h, cs) -> h cs rq k
    Nothing      -> app rq k

-- | Routes requests to 'Handler's via a routing trie, passing along
-- captured path parameters. A prefix of the request path must match a route in
-- order for the associated handler function to be called. Thereby the route
-- for the longest matching prefix is chosen. If no route matches a
-- prefix of the request path, the request is forward to the 'App'lication
-- given as second argument.
--
-- /Note/: With prefix routing, the 'pathInfo' of the 'Request' passed to
-- a handler contains only the (unmatched) suffix of the request path, enabling
-- nested / chained routing with multiple routing tries.
routePrefix :: Monad m => RoutingTrie m -> App m -> App m
routePrefix rt app rq k = case Trie.matchPrefix (pathInfo rq) rt of
    Just (h, cs, str') -> h cs (rq { pathInfo = str' }) k
    Nothing            -> app rq k

------------------------------------------------------------------------------
-- Paths

-- | The names and types of the variables captured in a 'Path'.
type Vars = [(Symbol,Type)]

-- | A parameter of type @a@ with (type-level) name @s@.
type Var s a = '(s,a)

-- | An unnamed parameter of type @a@.
type Some a = Var "" a

-- | A path of a 'Route', indexed by the names and types of
-- the captured variables.
--
-- Paths are constructed using 'str', 'var', 'some', glued
-- together by './' and terminated by 'end', e.g.
--
-- >>> let p = str "a" ./ var @"b" @Int ./ str "c" ./ some @Text ./ end
-- >>> :t p
-- p :: Path '[Var "b" Int, Some Text]
--
-- Two different paths are /overlapping/ iff their underlying 'Pattern's,
-- as given by 'pathPattern', are overlapping. The preference given to
-- routes based on overlapping paths is given by the preference between
-- the overlapping patterns (see the documentation for 'Pattern's).
data Path :: Vars -> Type where
    Val :: Text
        -> Path vars
        -> Path vars
    Var :: forall s a vars. (KnownSymbol s, Eq a, FromHttpApiData a)
        => Proxy# s
        -> Proxy# a
        -> Path vars
        -> Path (Var s a ': vars)
    End :: Path '[]

-- | Equality for paths indexed by the same 'Vars' is subsumed by the
-- structural equality '=~='.
instance Eq (Path vars) where
    p1 == p2 = p1 =~= p2

-- | Shows paths with a leading "/", whereby 'str'ings stand in
-- for themselves, unnamed 'var'iables are represented by a "*"
-- and named variables are represented by a ":" followed by the
-- name of the variable.
--
-- >>> let p = str "a" ./ var @"b" @Int ./ str "c" ./ some @Text ./ end
-- >>> p
-- /a/:b/c/*
instance Show (Path vars) where
    show End         = ""
    show (Val s   p) = showString "/" . showString (Text.unpack s) . shows p $ ""
    show (Var s _ p) = showString "/" . (
                        let s' = symbolVal' s
                        in if null s'
                            then showString "*"
                            else showString ":" . showString s'
                        ) . shows p $ ""

-- | Structural equality of paths.
--
-- @p =~= p'@ \(\iff\) @pathPattern p == pathPattern p'@
--
(=~=) :: Path vars -> Path vars' -> Bool
(=~=) End         End          = True
(=~=) (Val s   p) (Val s'  p') = s == s' && p =~= p'
(=~=) (Var _ _ p) (Var _ _ p') = p =~= p'
(=~=) _                      _ = False

-- | Capture a parameter as a named variable, e.g.
--
-- >>> let p = var @"name" @Text ./ end
-- >>> :t p
-- p :: Path '[Var "name" Text]
--
-- The type-level variable name can serve at least two purposes:
--
--   * It allows to disambiguate multiple parameters of the same type in the
--     path for extra type safety (i.e. against mixing up the order of
--     parameters).
--   * The name is made available at runtime when parsing of a parameter
--     fails in the form of an 'InvalidParam' error, enabling its use in
--     error responses, logs, etc.
--
var :: forall s a vars. (KnownSymbol s, Eq a, FromHttpApiData a)
    => Path vars -> Path (Var s a ': vars)
var = Var proxy# proxy#

-- | Capture a parameter as an unnamed variable, e.g.
--
-- >>> let p = some @Text ./ end
-- >>> :t p
-- p :: Path '[Some Text]
some :: forall a vars. (Eq a, FromHttpApiData a)
    => Path vars -> Path (Some a ': vars)
some = var @""

-- | Match a fixed string, capturing nothing, e.g.
--
-- >>> let p = str "tmp" ./ end
-- >>> :t p
-- p :: Path '[]
str :: Text -> Path vars -> Path vars
str = Val

-- | Right-associative infix operator for constructing 'Path's:
--
-- >>> let p = str "a" ./ some @Int ./ var @"y" @Int ./ end
-- >>> :t p
-- p :: Path '[Some Int, Var "y" Int]
--
(./) :: (Path vars -> Path vars') -> Path vars -> Path vars'
(./) f p = f p
infixr 5 ./

-- | Mark the end of a path.
end :: Path '[]
end = End

-- | The underlying structural 'Pattern' of a 'Path'.
pathPattern :: Path vars -> Pattern Text
pathPattern = go Seq.empty
  where
    go :: Pattern Text -> Path vars -> Pattern Text
    go pat (Val s   p) = EqStr s <| go pat p
    go pat (Var _ _ p) = AnyStr  <| go pat p
    go pat End         = pat

------------------------------------------------------------------------------
-- SomePath

-- | A path with existentially quantified variables.
data SomePath = forall vars. SomePath (Path vars)

deriving instance Show SomePath

instance Eq SomePath where
    (SomePath p1) == (SomePath p2) = p1 =~= p2

------------------------------------------------------------------------------
-- Routes

-- | A route whose handler is run if the path is a match for a request path.
-- The handler function is thereby given the captured and parsed 'Params'.
data Route m = forall vars. Route
    { routePath :: Path vars
    , routeHandler :: Params vars -> App m
    , routeInvalidParam :: Request -> InvalidParam -> m Response
    }

instance Eq (Route m) where
    (Route p1 _ _) == (Route p2 _ _) = p1 =~= p2

instance Show (Route m) where
    show (Route p _ _) = show p

-- | Define a 'Route' with the given path and handler, using default
-- values for other arguments.
defRoute :: Monad m => Path vars -> (Params vars -> App m) -> Route m
defRoute p h = Route p h defInvalidParam

-- | Default handler for invalid parameters.
defInvalidParam :: Monad m => Request -> InvalidParam -> m Response
defInvalidParam _rq (InvalidParam n v e) =
    return $ responseLBS code hdrs body
  where
    code = status400
    hdrs = [(hContentType, "text/plain; charset=utf-8")]
    body = LazyText.encodeUtf8 . toLazyText $
           fromString "Invalid parameter "
        <> fromString "["
            <> (if not (null n)
                then fromString n <> fromString "="
                else mempty)
            <> fromText v
        <> fromString "], "
        <> fromText e

-- | Compile a list of 'Route's into a 'RoutingTrie'.
compileRoutes :: Monad m => [Route m] -> RoutingTrie m
compileRoutes = Trie.fromAssocList . map compileRoute

-- | Compile a 'Route' into a pair of a 'Pattern' and a 'Handler',
-- suitable for insertion into a 'RoutingTrie'.
compileRoute :: forall m. Monad m => Route m -> (Pattern Text, Handler m)
compileRoute (Route p h f) = (pathPattern p, handler)
  where
    handler cs = case parseParams p cs of
        Right args            -> h args
        Left (ParseInvalid x) -> \rq k -> f rq x >>= k
        Left ParseIncomplete  ->
            -- Note [Incomplete parse]
            error "wai-route: incomplete parse: missing captures"

{- Note [Incomplete parse]
~~~~~~~~~~~~~~~~~~~~~~~~~~
It is a central property of the underlying pattern trie that if @p@ is a pattern
in a trie, then a successful match on that pattern yields exactly @n@ captured
values, where @n@ is the number of captures in the pattern. Since the number of
captures in a pattern is equal to the number of variables in a path by
definition of 'Path' and 'pathPattern', an incomplete parse at this point cannot
happen, given the trie works correctly.
-}

------------------------------------------------------------------------------
-- Parameter Parsing

-- | A heterogenous list of parameters obtained from captured variables.
data Params :: Vars -> Type where
    Nil   :: Params '[]
    (:::) :: forall s a vars. Eq a
          => a -> Params vars -> Params (Var s a ': vars)

infixr 5 :::
deriving instance Eq (Params vars)

data InvalidParam = InvalidParam
    { invalidParamName  :: String
    , invalidParamValue :: Text
    , invalidParamMsg   :: Text
    } deriving (Eq, Show, Read)

data ParseError
    = ParseIncomplete
        -- ^ The path contains more variables than the number of captures given.
    | ParseInvalid InvalidParam
        -- ^ A parameter failed to parse into the type expected by the
        -- corresponding variable of the path.
    deriving (Eq, Show, Read)

-- | Parse a sequence of captures into a heterogeneous list of 'Params'
-- according to the 'Vars' in the given 'Path'. The number of captures
-- given must be at least as large as the number of variables in the path,
-- in order for the parse to succeed.
parseParams :: Path vars -> Seq (Capture Text) -> Either ParseError (Params vars)
parseParams End       _     = Right Nil
parseParams (Val _ p) cs    = parseParams p cs
parseParams _         Empty = Left ParseIncomplete
parseParams (Var s (_ :: Proxy# a) p) (Capture c :<| cs) =
    case parseUrlPiece @a c of
        Right a -> (a :::) <$> parseParams p cs
        Left  e -> Left $! ParseInvalid (InvalidParam (symbolVal' s) c e)

------------------------------------------------------------------------------
-- Utilities

-- | Get and parse the request method as a 'StdMethod'.
getStdMethod :: Request -> Either ByteString StdMethod
getStdMethod = parseMethod . requestMethod

-- | Dispatch a request to an application based on the standardised
-- HTTP request methods (verbs). If the request method is not a
-- standard method, 'app405' is called.
byStdMethod :: (StdMethod -> App m) -> App m
byStdMethod f rq k = case getStdMethod rq of
    Left  _ -> app405 rq k
    Right m -> f m rq k

-- | Get and parse a query parameter by its name, assuming UTF-8
-- encoding of the value. If the query parameter is not present in the
-- request or has an empty value, 'Nothing' is returned.
--
-- If the parameter name is known to contain only ASCII characters (the most
-- common case), this function is more efficient than 'getQueryParam\'', since
-- query parameter names are plain 'ByteString's in the WAI.
getQueryParam :: FromHttpApiData a => Request -> ByteString -> Maybe (Either InvalidParam a)
getQueryParam rq key = case Prelude.lookup key (queryString rq) of
    Nothing        -> Nothing -- key not present
    Just Nothing   -> Nothing -- empty value
    Just (Just bs) ->
        let val = decodeUtf8With lenientDecode bs
        in case parseQueryParam val of
            Right a -> Just (Right a)
            Left  e -> Just (Left (InvalidParam (C8.unpack key) val e))

-- | Like 'getQueryParam' but supports UTF-8 encoded names of query parameters.
getQueryParam' :: FromHttpApiData a => Request -> Text -> Maybe (Either InvalidParam a)
getQueryParam' rq = getQueryParam rq . encodeUtf8

data InvalidHeader = InvalidHeader
    { invalidHeaderName  :: HeaderName
    , invalidHeaderValue :: ByteString
    , invalidHeaderMsg   :: Text
    } deriving (Eq, Show, Read)

getHeader :: FromHttpApiData a => Request -> HeaderName -> Maybe (Either InvalidHeader a)
getHeader rq h = case Prelude.lookup h (requestHeaders rq) of
    Nothing -> Nothing
    Just bs -> case parseHeader bs of
        Right a -> Just (Right a)
        Left  e -> Just (Left (InvalidHeader h bs e))

-- | An application that always yields an empty 404 Not Found response.
app404 :: App m
app404 _rq k = k $ responseLBS status404 [] mempty

-- | An application that always yields an empty 405 Method Not Allowed response.
app405 :: App m
app405 _rq k = k $ responseLBS status405 [] mempty

