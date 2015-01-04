{-
 - Route.hs
 - By Steven Smith
 -}

{-# LANGUAGE OverloadedStrings #-}

-- | 'Route' is a data type defining the hierarchy of web routes for a web
-- application.
--
-- A web route is a URI path to a resource. Consider the following URL:
--
-- > http://www.example.com/users/103/history?query=foo&s=bar
--
-- The route for this URL would be \"@users\/103\/history@\". When a request is
-- received it is split up into path segments and walked down the tree matching
-- segments (and potentially back-tracking) until it reaches an 'EndPoint' that
-- matches on the request method (@GET@, @PUT@, etc.). An 'EndPoint' then
-- generates the response to send back to the client.
--
-- Web routes are naturally hierarchical, so the 'Route' data type
-- represents a tree. It fully supports back-tracking, so branches that end
-- without matching an 'EndPoint' will simply back-track up the tree and
-- continue attempting to match.
module Web.DumpTruck.Route
( Route
, route
, routeEnd
, capture
, captureInt
, get
, post
, put
, delete
, matchAny
, remainingPath
, directory
, directory'
, mkDumpTruckApp
)
where

import Web.DumpTruck.EndPoint

import Data.ByteString.Lazy (ByteString)
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text.Read (decimal)
import Control.Monad
import Control.Monad.Free
import Control.Monad.Free.Church
import Network.Wai
import Network.HTTP.Types

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Network.HTTP.Types as H

data RouteF a = Route Text a a
              | RouteEnd a a
              | Capture (Text -> a) a
              | CaptureInt (Int -> a) a
              | Terminal Method (EndPoint IO RespBuilder) a
              | TerminalAny (EndPoint IO RespBuilder)
              | RemainingPath ([Text] -> a)
              | BackTrack a

instance Functor RouteF where
    fmap f (Route t c n) = Route t (f c) (f n)
    fmap f (RouteEnd c n) = RouteEnd (f c) (f n)
    fmap f (Capture g n) = Capture (f . g) (f n)
    fmap f (CaptureInt g n) = CaptureInt (f . g) (f n)
    fmap f (Terminal m r n) = Terminal m r (f n)
    fmap f (TerminalAny r) = TerminalAny r
    fmap f (RemainingPath g) = RemainingPath (f . g)
    fmap f (BackTrack n) = BackTrack (f n)

-- | The 'Route' web routing tree data type. A DumpTruck web application is a
-- 'Route' routing tree with some number of 'EndPoint' leaves that produce a
-- 'Response'. 'Route' is a 'Monad', and @do@ notation is the prefered means of
-- building up 'Route' values.
type Route = F RouteF

-------------------------------------------------------------------------------
-- Route Smart Constructors
-------------------------------------------------------------------------------

-- | Attempts to match the current path segment with the given 'Text'. If
-- successful, it will consume the path segment and continue down the given
-- 'Route'. Otherwise, matching will fall through to the next 'Route'.
route :: Text -- ^ The path segment to match on. Multiple path segments can be
              -- specified at once by separating them with "@\/@" characters.
              -- Leading and trailing "@\/@" characters are ignored.
      -> Route () -- ^ The route to take if matching succeeds.
      -> Route ()
route t d = foldr route' d $ filter (/= "") (T.splitOn "/" t)
  where
    route' t d = wrap $ Route t (d >> liftF (BackTrack ())) (return ())

-- | Will only match if there are no more path segments to consider. Otherwise,
-- matching will fall through to the next 'Route'.
routeEnd :: Route () -- ^ The route to take if matching succeeds.
         -> Route ()
routeEnd d = wrap $ RouteEnd d (return ())

-- | Matches any path segment text so long as there is a path segment to match
-- against. If matching succeeds the path segment is consumed and passed to the
-- given function as an argument to produce the route to continue on. Otherwise,
-- matching will fall through to the next 'Route'.
capture :: (Text -> Route ()) -- ^ The function called when there is a path
                              -- segment to capture, feeding it to the
                              -- function to produce the route to take.
        -> Route ()
capture f = wrap $ Capture ((>> liftF (BackTrack ())) . f) (return ())

-- | Matches any path segment text that can be parsed into an 'Int'. If matching
-- succeeds the path segment is consumed and passed to the given function as an
-- 'Int' value to produce the 'Route' to continue on. Otherwise, matching will
-- fall through to the next 'Route'.
captureInt :: (Int -> Route ()) -- ^ The function called when there is a
                                -- path segment and it can be parsed as an
                                -- 'Int'. The parsed value will be fed into
                                -- this function to produce the route to
                                -- take.
           -> Route()
captureInt f = wrap $ CaptureInt ((>> liftF (BackTrack ())) . f) (return ())

-- | Matches if the request method is @GET@. This is a terminal node. If
-- matching succeeds, the 'EndPoint' will be executed to generate the final
-- 'Response' and matching will end.
get :: EndPoint IO RespBuilder -> Route ()
get r = liftF $ Terminal methodGet r ()

-- | Matches if the request method is @POST@. This is a terminal node. If
-- matching succeeds, the 'EndPoint' will be executed to generate the final
-- 'Response' and matching will end.
post :: EndPoint IO RespBuilder -> Route ()
post r = liftF $ Terminal methodPost r ()

-- | Matches if the request method is @PUT@. This is a terminal node. If
-- matching succeeds, the 'EndPoint' will be executed to generate the final
-- 'Response' and matching will end.
put :: EndPoint IO RespBuilder -> Route ()
put r = liftF $ Terminal methodPut r ()

-- | Matches if the request method is @DELETE@. This is a terminal node. If
-- matching succeeds, the 'EndPoint' will be executed to generate the final
-- 'Response' and matching will end.
delete :: EndPoint IO RespBuilder -> Route ()
delete r = liftF $ Terminal methodDelete r ()

-- | Matches if the request method is the one provided. This is a terminal node.
-- If matching succeeds, the 'EndPoint' will be executed to generate the final
-- 'Response' and matching will end.
method :: Method -> EndPoint IO RespBuilder -> Route ()
method m r = liftF $ Terminal m r ()

-- | Matching will always succeed. This is a terminal node. The 'EndPoint'
-- will always be executed to generate the final 'Response' and matching will
-- end.
matchAny :: EndPoint IO RespBuilder -> Route ()
matchAny = liftF . TerminalAny

-- | Produces the remaining path to match against as a list of 'Text' path
-- segments.
remainingPath :: Route [Text]
remainingPath = liftF $ RemainingPath id

-- Derived

-- | Attempts to match the given 'Text' path segment in the same manner as
-- 'route'. If matching succeeds and the request method is @GET@, then it will
-- take the given path segment and the remaining request path, append them, and
-- attempt to serve the file at the resulting file path. Will give a @404 Not
-- Found@ if the file does not exist.
--
-- This will also filter out any path segments that are "@..@" because this can
-- be a potential security hole and is essentially never desired behavior.
directory :: Text -> Route ()
directory fp = directory' fp fp

-- | This is a variation of 'directory' in which the route to match and the base
-- path of the file to serve can be different.
directory' :: Text -- ^ The route to match
           -> Text -- ^ The base path for the path to serve
           -> Route ()
directory' rt fp = route rt $ do path <- remainingPath
                                 get $ go path
  where
    go = file . T.unpack . T.concat . intersperse "/" . (fp :) . filter (/= "..")

-------------------------------------------------------------------------------
-- Running DumpTruck Apps
-------------------------------------------------------------------------------

-- | Converts a DumpTruck app into a WAI 'Application', which can be run on any
-- web server that can serve WAI 'Application's.
mkDumpTruckApp :: Route a -> Application
mkDumpTruckApp d req c = go [] p (fromF d)
  where
    p = pathInfo req
    method = requestMethod req
    cont = generateResponse req >=> c
    go b p' (Pure _) = cont notFound
    go b p' (Free op) = go' b p' op
    go' back [] (Route t c n) = go back [] n
    go' back path@(x:xs) (Route t c n) =
        if x == t
        then go (x:back) xs c
        else go back path n
    go' back [] (RouteEnd c n) = go back [] c
    go' back path (RouteEnd c n) = go back path n
    go' back [] (Capture f n) = go back [] n
    go' back (x:xs) (Capture f n) = go (x:back) xs (f x)
    go' back [] (CaptureInt f n) = go back [] n
    go' back path@(x:xs) (CaptureInt f n) =
        case decimal x of
            Left _ -> go back path n
            Right (i,_) -> go (x:back) xs (f i)
    go' back path (Terminal m r n) =
        if m == method
        then cont r
        else go back path n
    go' back path (TerminalAny r) = cont r
    go' back path (RemainingPath f) = go back path (f path)
    go' [] path (BackTrack n) = go [] path n -- Should probably fail somehow
    go' (x:xs) path (BackTrack n) = go xs (x:path) n
