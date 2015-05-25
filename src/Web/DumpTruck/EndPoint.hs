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
, getState
, addHeader
, buildResponse
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
, withFormData
, withJson
, withRequestData
)
where

import Web.DumpTruck.Date
import Web.DumpTruck.Form
import Web.DumpTruck.RequestData

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Aeson hiding (json)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Maybe
import Data.Sequence (Seq, (|>))
import Data.Time.Clock (UTCTime)
import Network.HTTP.Types
import Network.Wai
import System.Directory

import qualified Data.ByteString as B
import qualified Data.Sequence as S

-- | An 'EndPoint' is the action to perform after successfully matching a
-- 'DumpTruck' route. This is a variant of the writer monad transformer, and is
-- used to set HTTP headers, set the HTTP status, and eventually produce the
-- final 'Response'.
newtype EndPoint s m a = EndPoint {
    runEndPoint :: Request -> Seq Header -> s
                -> m (Either Response (a, Seq Header))
}

instance Functor f => Functor (EndPoint s f) where
    fmap f (EndPoint m) = EndPoint (\r hs s -> (fmap . fmap) go (m r hs s))
      where
        go (a,hs) = (f a, hs)

-- TODO: Figure out why this Functor constraint is necessary
instance (Functor m, Monad m) => Applicative (EndPoint s m) where
    pure a = EndPoint (\_ hs _ -> return (Right (a, hs)))
    (EndPoint m1) <*> (EndPoint m2) = EndPoint go
      where
        go r hs s = do
            eith <- m1 r hs s
            case eith of
                Left resp -> return (Left resp)
                Right (f, hs') -> do
                    eith' <- m2 r hs' s
                    case eith' of
                        Left resp' -> return (Left resp')
                        Right (a, hs'') -> return (Right (f a, hs''))

instance Monad m => Monad (EndPoint s m) where
    return a = EndPoint (\_ hs _ -> return (Right (a, hs)))
    (EndPoint m) >>= k = EndPoint $ \r hs s -> do
        eith <- m r hs s
        case eith of
            Left resp -> return (Left resp)
            Right (a, hs') -> do
                eith' <- runEndPoint (k a) r hs' s
                case eith' of
                    Left resp -> return (Left resp)
                    Right (b, hs'') -> return (Right (b, hs''))

instance MonadTrans (EndPoint s) where
    lift m = EndPoint $ \r hs s -> do
        a <- m
        return (Right (a, hs))

instance MonadIO m => MonadIO (EndPoint s m) where
    liftIO = lift . liftIO

getRequest :: Monad m => EndPoint s m Request
getRequest = EndPoint (\r hs s -> return (Right (r, hs)))

-- | Retrieves the app-wide environment value.
getState :: Monad m => EndPoint s m s
getState = EndPoint (\_ hs s -> return (Right (s, hs)))

-- | Adds the given HTTP 'Header' to the final 'Response'.
addHeader :: Monad m => Header -> EndPoint s m ()
addHeader h = EndPoint (\_ hs _ -> return (Right ((), hs |> h)))

-- | Given a function that takes a list of 'Header's and produces a 'Response',
-- produces an 'EndPoint' that feeds the current set of 'Header's to produce the
-- 'Response' for this request.
buildResponse :: Monad m => ([Header] -> Response) -> EndPoint s m a
buildResponse f = EndPoint (\r hs _ -> return (Left (f (toList hs))))

-- | Generates the final 'Response' given the 'Request' and an 'EndPoint'. End
-- users should never have to use this function.
generateResponse :: Functor m => Request -> s -> EndPoint s m a -> m Response
generateResponse req s m = fmap go (runEndPoint m req S.empty s)
  where
    go (Left resp) = resp
    go (Right _) = responseLBS internalServerError500 [] "Internal Server Error"

-- | Produces an 'EndPoint' to serve a file at the given 'FilePath'. Will
-- automatically handle caching based on the file's modification timestamp.
file :: FilePath -> EndPoint s IO a
file fp = do
    exists <- liftIO $ doesFileExist fp
    if exists then do
        readable <- liftIO $ fmap readable (getPermissions fp)
        if readable then do
            modDate <- liftIO $ getModificationTime fp
            cacheOnMod (GmtTime modDate) (buildResponse resp)
        else notFound
    else notFound
  where
    resp hs = responseFile ok200 hs fp Nothing

-- | Enables caching for the given 'EndPoint' based on the given 'UTCTime'
-- timestamp. This means if a client sends an @If-Modified-Since@ header with a
-- timestamp that is not older than the given timestamp the server will send a
-- @304 Not Modified@ 'Response' instead of the normal 'Response'.
cacheOnMod :: GmtTime -> EndPoint s IO a -> EndPoint s IO a
cacheOnMod t r = do
    addHeader ("Last-Modified", gmtToByteString t)
    addHeader ("Cache-Control", "max-age=3600")
    cached <- withRequestData (noCache <|> modSince <|> pure False)
    if cached
    then buildResponse (\hs -> responseLBS notModified304 hs "")
    else r
  where
    noCache = containingString "no-cache" (getHeader "Cache-Control")
        *> pure False
    modSince = do
        modTime <- asCaptureable (getHeader "If-Modified-Since")
        return (modTime == t)

-- | Enables caching for the given 'EndPoint' based on the given HTTP ETag.
-- This means if a client sends an @If-None-Match@ header with an ETag that is
-- equivalent to the given ETag the server will send a @304 Not Modified@
-- 'Response' instead of the normal 'Response'.
cacheOnEtag :: B.ByteString -> EndPoint s IO a -> EndPoint s IO a
cacheOnEtag t r = do
    addHeader ("ETag", t)
    addHeader ("Cache-Control", "max-age=3600")
    cached <- withRequestData (etag <|> pure False)
    if cached
    then buildResponse (\hs -> responseLBS notModified304 hs "")
    else r
  where
    etag = do
        e <- getHeader "If-Modified-Since"
        return (e == t)

-- Endpoint smart constructors

-- | Produces an 'EndPoint' that simply has the given lazy 'ByteString' as the
-- 'Response' body.
raw :: Monad m => ByteString -> EndPoint s m a
raw bs = buildResponse (\hs -> responseLBS ok200 hs bs)

-- | Produces an 'EndPoint' that simply has the given lazy 'ByteString' as the
-- 'Response' body while setting the @Content-Type@ to @application/json@.
rawJson :: Monad m => ByteString -> EndPoint s m a
rawJson bs = do
    addHeader (hContentType, "application/json")
    buildResponse (\hs -> responseLBS ok200 hs bs)

-- | Produces an 'EndPoint' that has the given value encoded as JSON as the
-- 'Response' body while setting the @Content-Type@ to @application/json@.
json :: (ToJSON a, Monad m) => a -> EndPoint s m b
json a = do
    addHeader (hContentType, "application/json")
    buildResponse (\hs -> responseLBS ok200 hs (encode a))

-- | An 'EndPoint' that simply returns a @404 Not Found@ 'Response'.
notFound :: Monad m => EndPoint s m a
notFound = buildResponse (\hs -> responseLBS notFound404 hs "Not found")

-- | An 'EndPoint' that returns a @307 Temporary Redirect@ 'Response' with the
-- given URL as a strict 'B.ByteString' as the redirect destination.
redirect :: Monad m => B.ByteString -> EndPoint s m a
redirect bs = do
    addHeader (hLocation, bs)
    buildResponse
        (\hs -> responseLBS temporaryRedirect307 hs "Temporary redirect")

-- | An 'EndPoint' that returns a @301 Moved Permanently@ 'Response' with the
-- given URL as a strict 'B.ByteString' as the redirect destination.
redirectPermanent :: Monad m => B.ByteString -> EndPoint s m a
redirectPermanent bs = do
    addHeader (hLocation, bs)
    buildResponse
        (\hs -> responseLBS movedPermanently301 hs "Moved permanently")

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
methodNotAllowed :: Monad m => EndPoint s m a
methodNotAllowed = buildResponse (\hs -> responseLBS methodNotAllowed405 hs
    "Method not allowed")

-- | Will attempt to parse the request body as URL-encoded form data and- if
-- successful- will then feed that form data to the given function to produce an
-- 'EndPoint'. If parsing fails, it will return @400 Bad Request@.
withFormData :: ([(B.ByteString, B.ByteString)] -> EndPoint s IO a)
             -> EndPoint s IO a
withFormData f = do
    req <- getRequest
    maybeForm <- liftIO (parseFormData req)
    case maybeForm of
        Nothing -> buildResponse badRequest
        Just a  -> f a
  where
    badRequest hs = responseLBS badRequest400 hs "Invalid request body"

-- | Will attempt to parse the request body as JSON and- if successful- will
-- then feed that JSON data converted into the appropriate type to the given
-- function to produce an 'EndPoint'. If parsing fails, it will return @400 Bad
-- Request@.
withJson :: FromJSON a => (a -> EndPoint s IO b) -> EndPoint s IO b
withJson f = do
    req <- getRequest
    maybeA <- liftIO (parseFormJson req)
    case maybeA of
        Nothing -> buildResponse badRequest
        Just a  -> f a
  where
    badRequest hs = responseLBS badRequest400 hs "Invalid request body"

-- | Runs the given 'RequestData' with the available data from the request.
-- 'RequestData' actions that reference the request body will consume it,
-- meaning that subsequent 'withRequestData' calls to 'RequestData' actions that
-- pull the request body will instead get nothing.
--
-- If running the 'RequestData' fails then it will return @400 Bad Request@.
-- This behavior can be bypassed by wrapping the 'RequestData' with 'optional'
-- from 'Control.Applicative'.
withRequestData :: RequestData a -> EndPoint s IO a
withRequestData rd = do
    req <- getRequest
    maybeA <- liftIO (runRequestData req rd)
    case maybeA of
        Nothing -> buildResponse badRequest
        Just a -> return a
  where
    badRequest hs = responseLBS badRequest400 hs "Bad Request"
