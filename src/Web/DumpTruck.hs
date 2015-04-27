{-
 - DumpTruck.hs
 - By Steven Smith
 -}

-- | DumpTruck is a micro web framework. It is built up from two components:
-- 'Route's and 'EndPoint's.
--
-- 'Route's describe the @URI@ heirarchy for a web site. They are based around
-- matching path fragments. The recommended way to build up 'Route's is through
-- @do@ notation as 'Route' is a 'Monad'.
--
-- 'EndPoint's represent monadic actions to be executed to produce the final
-- 'Response' once 'Route' matching has reached a "terminal node". A terminal
-- node optionally matches on the 'Request' 'Method' and contains an associated
-- 'EndPoint'.
--
-- Once a 'Route' has been built with some number of 'EndPoint's it can be
-- converted into a @WAI@ 'Application' and run on a @WAI@ handler
-- (<http://hackage.haskell.org/package/warp Warp> is recommended) through
-- 'mkDumpTruckApp'. This will produce a web application server using the
-- 'Route' to match @URI@s and serve 'Response's from its 'EndPoint's.
--
-- Consider this simple web app (assuming the language extension
-- @OverloadedStrings@ is enabled):
--
-- @
-- myApp :: Route ()
-- myApp = matchAny (raw "Hello!")
-- @
--
-- 'matchAny' is a smart constructor for a 'Route' terminal node that will
-- always succeed in matching, executing the 'EndPoint' passed to it. 'raw'
-- constructs an 'EndPoint' that will simply respond with the lazy 'ByteString'
-- passed to it.  Thus, this DumpTruck app will always return the string
-- "Hello!" regardless of 'Request' 'Method' or @URI@. Now consider a slightly
-- more complicated app:
--
-- @
-- myApp :: Route ()
-- myApp = do get (raw "HTTP GET")
--            matchAny (raw "Not HTTP GET")
-- @
--
-- 'get' is a smart constructor for a 'Route' terminal node that will match if
-- the 'Request' 'Method' is @GET@. If matching fails for it, matching will
-- continue to the next node. Thus, the app will return "HTTP GET" if the
-- 'Request' 'Method' is @GET@, and "Not HTTP GET" otherwise. Similar smart
-- constructors exist:
--
-- @
-- myApp :: Route ()
-- myApp = do get (raw "HTTP GET")
--            post (raw "HTTP POST")
--            put (raw "HTTP PUT")
--            delete (raw "HTTP DELETE")
-- @
--
-- This app will match against each of the 'Request' 'Method's and execute the
-- corresponding 'EndPoint' if matching succeeds. This app doesn't have a
-- 'matchAny' node at the end, so matching can potentially fail. In this case a
-- @404 Not Found@ 'Response' is returned.
--
-- So far all the apps have ignored the @URI@. This app matches against a
-- specific path segment:
--
-- @
-- myApp :: Route ()
-- myApp = do
--     route "foo" $ do
--         matchAny (raw "Matched foo")
--     matchAny (raw "Didn't match foo")
-- @
--
-- This app will match the @URI@ "\/foo" and return the string "Matched foo" in
-- this case, or return "Didn't match foo" otherwise. This will also match the
-- @URI@ "\/foo\/bar". To cap the matching @URI@ use 'routeEnd':
--
-- @
-- myApp :: Route ()
-- myApp = do
--     route "foo" $ do
--         routeEnd $ do
--             matchAny (raw "Matched foo exactly")
--     matchAny (raw "Didn't match foo")
-- @
--
-- Now the app will match the @URI@ "\/foo" but not "\/foo\/bar". 'Route's will
-- consume the path segment when matching 'route', but will restore it if it
-- needs to backtrack. This allows arbitrary nesting of 'Route's to create full
-- @URI@ paths.
--
-- @
-- myApp :: Route ()
-- myApp = do
--     route "foo" $ do
--         route "bar" $ do
--             routeEnd $ do
--                 matchAny (raw "Matched exactly \/foo\/bar")
--         route "baz" $ do
--             matchAny (raw "Matched at least \/foo\/baz")
--     route "foo" $ do
--         route "bar" $ do
--             route "foobar" $ do
--                 matchAny (raw "Matched \/foo\/bar\/foobar")
--     route "quux" $ do
--         matchAny (raw "Matched /quux")
-- @
--
-- This can be shortened by using fewer @do@ blocks and combining redundant
-- segments:
--
-- @
-- myApp :: Route ()
-- myApp = do
--     route "foo" $ do
--         route "bar" $ do
--             routeEnd $ matchAny (raw "Matched exactly \/foo\/bar")
--             route "foobar" $ matchAny (raw "Matched \/foo\/bar\/foobar")
--         route "baz $ matchAny (raw "Matched at least \/foo\/baz")
--     route "quux" $ matchAny (raw "Matched \/quux")
-- @
--
-- Multiple 'route' nodes can also be compressed by simply including multiple
-- segments delineated by "\/":
--
-- @
-- myApp :: Route ()
-- myApp = do
--     route "foo\/bar" . routeEnd $ matchAny (raw "Matched exactly \/foo\/bar")
--     route "foo\/bar\/foobar" $ matchAny (raw "Matched \/foo\/bar\/foobar")
--     route "foo\/baz" $ matchAny (raw "Matched at least \/foo\/baz")
--     route "quux" $ matchAny (raw "Matched \/quux")
-- @
--
-- (Note that routes expressed in this manner are always fully expanded with no
-- attempt at merging branches, so this example will backtrack more than the one
-- preceding it.)
--
-- It is also possible to capture path segments as any type that is
-- 'Captureable'. Instances of this type class have a corresponding parser used
-- when attempting to capture. A number of common instances are provided, and
-- users can also write instances of 'Captureable' for their own data types and
-- use 'capture' with them normally. Matching will fail if the segment doesn't
-- exist or if parsing fails.
--
-- In addition to 'capture', there is 'captureInt' which is 'capture'
-- specialized to 'Int'. This can be useful as the polymorphic 'capture' can
-- cause type errors due to ambiguous types.
--
-- @
-- import qualified Data.ByteString.Lazy as LBS
--
-- myApp :: Route ()
-- myApp = do
--     route "text" $ capture $ \t ->
--         matchAny (raw $ "Captured: " LBS.++ t)
--     route "int" $ captureInt $ \i ->
--         if i > 3
--         then matchAny (raw "Greater than 3!")
--         else matchAny (raw "Less than 3!")
-- @
--
-- There are a number of other features available, as well as a set of
-- 'EndPoint's. See the rest of the documentation for more details.
--
module Web.DumpTruck
-- Web.DumpTruck.Route
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
-- Web.DumpTruck.EndPoint
, EndPoint
, addHeader
, setStatus
, RespBuilder
, generateResponse
, file
, cacheOnMod
, cacheOnEtag
, raw
, rawJson
, json
, notFound
, redirect
, redirectPermanent
, methodNotAllowed
)
where

import Web.DumpTruck.Route
import Web.DumpTruck.EndPoint
