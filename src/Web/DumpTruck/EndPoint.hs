{-
 - EndPoint.hs
 - By Steven Smith
 -}

{-# LANGUAGE OverloadedStrings #-}

-- | 'EndPoint' is a monad transformer that can be executed to produce a
-- 'Response' in some 'Monad' (almost always 'IO').
--
-- When a 'DumpTruck' web route is matched to a terminal node, an 'EndPoint'
-- is executed to generate the final 'Response'. 'EndPoint' itself is just a
-- custom 'WriterT' monad transformer to facilitate stateful setting of
-- 'Response' headers and the HTTP 'Status'.
module Web.DumpTruck.EndPoint
( EndPoint
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

import Web.DumpTruck.Date
import Web.DumpTruck.Form

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Aeson hiding (json)
import Data.ByteString.Lazy (ByteString)
import Data.List (intersperse)
import Data.Maybe
import Data.Time.Clock (UTCTime)
import Network.HTTP.Types
import Network.Wai
import System.Directory

import qualified Data.ByteString as B

-- | An 'EndPoint' is the action to perform after successfully matching a
-- 'DumpTruck' route. This is a variant of the writer monad transformer, and is
-- used to set HTTP headers, set the HTTP status, and eventually produce the
-- final 'Response'.
newtype EndPoint m a = EndPoint {
    runEndPoint :: Request -> m (a, Maybe Status, [Header] -> [Header])
}

instance Functor f => Functor (EndPoint f) where
    fmap f (EndPoint m) = EndPoint (\r -> fmap (\(a,b,c) -> (f a,b,c)) (m r))

instance Applicative f => Applicative (EndPoint f) where
    pure a = EndPoint (\r -> pure (a, Nothing, id))
    (EndPoint m1) <*> (EndPoint m2) = EndPoint (\r -> liftA2 combineStates (m1 r) (m2 r))

instance Monad m => Monad (EndPoint m) where
    return a = EndPoint (\r -> return (a, Nothing, id))
    (EndPoint m) >>= k = EndPoint $ \r -> do
        (a,s,hs) <- m r
        st <- runEndPoint (k a) r
        return (combineStates (id,s,hs) st)

instance MonadTrans EndPoint where
    lift m = EndPoint $ \r -> do
        a <- m
        return (a, Nothing, id)

instance MonadIO m => MonadIO (EndPoint m) where
    liftIO = lift . liftIO

combineStates :: ((a -> b), Maybe Status, [Header] -> [Header])
              -> (a, Maybe Status, [Header] -> [Header])
              -> (b, Maybe Status, [Header] -> [Header])
combineStates (f, s1, hs1) (a, s2, hs2) = (f a, s1 <|> s2, hs1 . hs2)

getRequest :: Monad m => EndPoint m Request
getRequest = EndPoint $ \r -> return (r, Nothing, id)

-- | Adds the given HTTP 'Header' to the final 'Response'.
addHeader :: Monad m => Header -> EndPoint m ()
addHeader h = EndPoint $ \r -> return ((), Nothing, (h:))

-- | Sets the HTTP 'Status' to the given value. If the 'Status' had already been
-- set, then it is overwritten with the new value.
setStatus :: Monad m => Status -> EndPoint m ()
setStatus s = EndPoint $ \r -> return ((), Just s, id)

-- | A type synonym for the final function to be called to produce the final
-- 'Response'.
type RespBuilder = Status -> [Header] -> Response

-- | Generates the final 'Response' given the 'Request' and an 'EndPoint'. End
-- users should never have to use this function.
generateResponse :: Functor m => Request -> EndPoint m RespBuilder -> m Response
generateResponse req m = fmap go (execEndPoint req m)
  where
    go (f, s, hs) = f s hs

execEndPoint :: Functor m => Request -> EndPoint m a -> m (a, Status, [Header])
execEndPoint req m = fmap go (runEndPoint m $ req)
  where
    go (a, s, hs) = (a, maybe ok200 id s, hs [])

-- | Produces an 'EndPoint' to serve a file at the given 'FilePath'. Will
-- automatically handle caching based on the file's modification timestamp.
file :: (Monad m, MonadIO m) => FilePath -> EndPoint m RespBuilder
file fp = do
    exists <- liftIO $ doesFileExist fp
    if exists then do
        readable <- liftIO $ fmap readable (getPermissions fp)
        if readable then do
            modDate <- liftIO $ getModificationTime fp
            cacheOnMod modDate (return resp)
        else notFound
    else notFound
  where
    resp s hs = responseFile s hs fp Nothing

-- | Enables caching for the given 'EndPoint' based on the given 'UTCTime'
-- timestamp. This means if a client sends an @If-Modified-Since@ header with a
-- timestamp that is not older than the given timestamp the server will send a
-- @304 Not Modified@ 'Response' instead of the normal 'Response'.
cacheOnMod :: Monad m => UTCTime -> EndPoint m RespBuilder
           -> EndPoint m RespBuilder
cacheOnMod t r = do
    addHeader ("Last-Modified", utcToGmtString t)
    addHeader ("Cache-Control", "max-age=3600")
    r' <- r
    req <- getRequest
    return $ go t r' req
  where
    go t rb req s hs = maybe (rb s hs) maybe304 ifMod
      where
        ifMod = do
            let reqHeaders = requestHeaders req
            guardNoCache reqHeaders
            m <- lookup "If-Modified-Since" reqHeaders
            parseGmtTimeToUTCTime m
        maybe304 ims = if t > ims
                     then rb s hs
                     else responseLBS notModified304 hs ""

-- | Enables caching for the given 'EndPoint' based on the given HTTP ETag.
-- This means if a client sends an @If-None-Match@ header with an ETag that is
-- equivalent to the given ETag the server will send a @304 Not Modified@
-- 'Response' instead of the normal 'Response'.
cacheOnEtag :: Monad m => B.ByteString -> EndPoint m RespBuilder
            -> EndPoint m RespBuilder
cacheOnEtag t r = do
    addHeader ("ETag", t)
    addHeader ("Cache-Control", "max-age=3600")
    r' <- r
    req <- getRequest
    return $ go t r' req
  where
    go t rb req s hs = maybe (rb s hs) maybe304 ifMod
      where
        ifMod = do
            let reqHeaders = requestHeaders req
            guardNoCache reqHeaders
            lookup "If-None-Match" reqHeaders
        maybe304 ims = if t /= ims
                     then rb s hs
                     else responseLBS notModified304 hs ""

-- | Given a set of HTTP 'Header's, will return 'Nothing' if the header
-- "@Cache-Control@" exists and contains the 'B.ByteString' "@no-cache@".
-- Otherwise returns 'Just' '()'.
guardNoCache :: [Header] -> Maybe ()
guardNoCache hs = case go of
    Nothing -> Just ()
    Just () -> Nothing
  where go = mapM_ go' $ filter ((== "Cache-Control") . fst) hs
        go' (_,c) = case B.breakSubstring "no-cache" c of
                         (_,y) | B.null y  -> Nothing
                               | otherwise -> Just ()

-- Endpoint smart constructors

-- | Produces an 'EndPoint' that simply has the given lazy 'ByteString' as the
-- 'Response' body.
raw :: Monad m => ByteString -> EndPoint m RespBuilder
raw bs = return r
  where
    r s hs = responseLBS s hs bs

-- | Produces an 'EndPoint' that simply has the given lazy 'ByteString' as the
-- 'Response' body while setting the @Content-Type@ to @application/json@.
rawJson :: Monad m => ByteString -> EndPoint m RespBuilder
rawJson bs = addHeader (hContentType, "application/json") >> return r
  where
    r s hs = responseLBS s hs bs

-- | Produces an 'EndPoint' that has the given value encoded as JSON as the
-- 'Response' body while setting the @Content-Type@ to @application/json@.
json :: (ToJSON a, Monad m) => a -> EndPoint m RespBuilder
json a = addHeader (hContentType, "application/json") >> return r
  where
    r s hs = responseLBS s hs (encode a)

-- | An 'EndPoint' that simply returns a @404 Not Found@ 'Response'.
notFound :: Monad m => EndPoint m RespBuilder
notFound = return r
  where
    r _ hs = responseLBS notFound404 hs "Not found"

-- | An 'EndPoint' that returns a @307 Temporary Redirect@ 'Response' with the
-- given URL as a strict 'B.ByteString' as the redirect destination.
redirect :: Monad m => B.ByteString -> EndPoint m RespBuilder
redirect bs = addHeader (hLocation, bs) >> return r
  where
    r _ hs = responseLBS temporaryRedirect307 hs "Temporary redirect"

-- | An 'EndPoint' that returns a @301 Moved Permanently@ 'Response' with the
-- given URL as a strict 'B.ByteString' as the redirect destination.
redirectPermanent :: Monad m => B.ByteString -> EndPoint m RespBuilder
redirectPermanent bs = addHeader (hLocation, bs) >> return r
  where
    r _ hs = responseLBS movedPermanently301 hs "Moved permanently"

-- | An 'EndPoint' that simply returns @405 Method Not Allowed@ 'Response'.
-- This is often used with 'matchAny' after matching on every available request
-- method, like so:
--
-- @
-- route "path" $ do
--     get doGet
--     post doPost
--     delete doDelete
--     matchAny methodNotAllowed
-- @
methodNotAllowed :: Monad m => EndPoint m RespBuilder
methodNotAllowed = return r
  where
    r _ hs = responseLBS methodNotAllowed405 hs "Method not allowed"

withJson :: FromJSON a => (a -> EndPoint IO RespBuilder) -> EndPoint IO RespBuilder
withJson f = do
    req <- getRequest
    maybeA <- liftIO (parseFormJson req)
    case maybeA of
        Nothing -> return badRequest
        Just a  -> f a
  where
    badRequest _ hs = responseLBS badRequest400 hs "Invalid request body"
