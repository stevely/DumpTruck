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
, options
, patch
, method
, matchAny
, remainingPath
, directory
, directory'
, mkDumpTruckApp
)
where

import Web.DumpTruck.Capture
import Web.DumpTruck.EndPoint

import Data.ByteString.Lazy (ByteString)
import Data.List (intersperse, stripPrefix)
import Data.Text (Text)
import Control.Applicative
import Control.Monad
import Network.Wai
import Network.HTTP.Types

import qualified Data.Text as T

-- | The 'Route' web routing tree data type. A DumpTruck web application is a
-- 'Route' routing tree with some number of 'EndPoint' leaves that produce a
-- 'Response'. 'Route' is a 'Monad', and @do@ notation is the prefered means of
-- building up 'Route' values.
newtype Route a = Route {
    runRoute :: [Text] -> Method -> Either (EndPoint IO ()) a
}

instance Functor Route where
    fmap f (Route r) = Route go
      where
        go ts m = fmap f (r ts m)

instance Applicative Route where
    pure x = Route go
      where
        go _ _ = Right x
    (Route f) <*> (Route r) = Route go
      where
        go ts m = let f' = f ts m
                      r' = r ts m
                   in f' <*> r'

instance Monad Route where
    return = pure
    (Route f) >>= k = Route go
      where
        go ts m = case f ts m of
            Left e -> Left e
            Right a -> (runRoute (k a)) ts m

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
route t (Route r) = Route go
  where
    go [] _ = return ()
    go ps m = case stripPrefix (filter (/= "") (T.splitOn "/" t)) ps of
        Nothing -> return ()
        Just xs -> r xs m

-- | Will only match if there are no more path segments to consider. Otherwise,
-- matching will fall through to the next 'Route'.
routeEnd :: Route () -- ^ The route to take if matching succeeds.
         -> Route ()
routeEnd (Route r) = Route go
  where
    go [] m = r [] m
    go _ _ = return ()

-- | Matches any path segment that can be parsed into a 'Captureable' type. If
-- parsing succeeds the path segment is consumed and passed to the given
-- function as the parsed value to produce the 'Route' to continue on.
-- Otherwise matching falls through to the next 'Route'.
--
-- Note: Since this function is polymorphic in the argument but not the result,
-- it may be necessary to include a type annotation in the given function to
-- force its argument type to be monomorphic.
capture :: Captureable a
        => (a -> Route ()) -- ^ The function called when there is a path segment
                           -- and it can be parsed into the appropriate type.
                           -- The parsed value will be fed into this function to
                           -- produce the next 'Route' to take.
        -> Route ()
capture f = Route go
  where
    go [] _ = return ()
    go (x:xs) m = case performCapture x of
        Nothing -> return ()
        Just a  -> (runRoute (f a)) xs m

-- | Matches any path segment that can be parsed into an 'Int'. If parsing
-- succeeds the path segment is consumed and passed to the given function as the
-- parsed value to produce the 'Route' to continue on. Otherwise matching falls
-- through to the next 'Route'.
--
-- Note: This function is just a monomorphic version of 'capture'.
captureInt :: (Int -> Route ()) -- ^ The function called when there is a path
                                -- segment and it can be parsed into an 'Int'.
                                -- The parsed value will be fed into this
                                -- function to produce the route to take.
           -> Route ()
captureInt = capture

-- | Matches if the request method is @GET@. This is a terminal node. If
-- matching succeeds, the 'EndPoint' will be executed to generate the final
-- 'Response' and matching will end.
get :: EndPoint IO () -> Route ()
get = method methodGet

-- | Matches if the request method is @POST@. This is a terminal node. If
-- matching succeeds, the 'EndPoint' will be executed to generate the final
-- 'Response' and matching will end.
post :: EndPoint IO () -> Route ()
post = method methodPost

-- | Matches if the request method is @PUT@. This is a terminal node. If
-- matching succeeds, the 'EndPoint' will be executed to generate the final
-- 'Response' and matching will end.
put :: EndPoint IO () -> Route ()
put = method methodPut

-- | Matches if the request method is @DELETE@. This is a terminal node. If
-- matching succeeds, the 'EndPoint' will be executed to generate the final
-- 'Response' and matching will end.
delete :: EndPoint IO () -> Route ()
delete = method methodDelete

-- | Matches if the request method is @OPTIONS@. This is a terminal node. If
-- matching succeeds, the 'EndPoint' will be executed to generate the final
-- 'Response' and matching will end.
options :: EndPoint IO () -> Route ()
options = method methodOptions

-- | Matches if the request method is @PATCH@. This is a terminal node. If
-- matching succeeds, the 'EndPoint' will be executed to generate the final
-- 'Response' and matching will end.
patch :: EndPoint IO () -> Route ()
patch = method methodPatch

-- | Matches if the request method is the one provided. This is a terminal node.
-- If matching succeeds, the 'EndPoint' will be executed to generate the final
-- 'Response' and matching will end.
method :: Method -> EndPoint IO () -> Route ()
method m e = Route go
  where
    go _ m'
        | m == m'   = Left e
        | otherwise = return ()

-- | Matching will always succeed. This is a terminal node. The 'EndPoint'
-- will always be executed to generate the final 'Response' and matching will
-- end.
matchAny :: EndPoint IO () -> Route ()
matchAny e = Route go
  where
    go _ _ = Left e

-- | Produces the remaining path to match against as a list of 'Text' path
-- segments.
remainingPath :: Route [Text]
remainingPath = Route go
  where
    go p _ = return p

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
mkDumpTruckApp (Route r) req cont = case r (pathInfo req) (requestMethod req) of
    Right _ -> cont' notFound
    Left e  -> cont' e
  where
    cont' = generateResponse req >=> cont
