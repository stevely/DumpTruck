{-
 - RequestData.hs
 - By Steven Smith
 -}
{-# LANGUAGE OverloadedStrings #-}

-- | 'RequestData' provides a common interface for pulling data from the various
-- sources of the 'Request'. Specifically, the following data sources are
-- available:
--
-- * Request body
-- * Query parameters
-- * Cookies
-- * Request headers (this includes the full, unparsed cookie data)
--
-- 'RequestData' will handle failure automatically, as well as parsing when
-- available. Execution of the 'RequestData' action will halt upon the first
-- failure. To let a data fetch/parse fail without halting the entire action,
-- wrap the 'RequestData' with 'optional' from 'Control.Applicative'.
--
-- A special note for request body operations needs to be made. The request body
-- is consumed when it is queried because it comes in as chunks and we may need
-- to block while waiting for the next chunk. This means if the request body is
-- queried twice at any point in executing an 'EndPoint' then the second query
-- with be given nothing.
--
-- An important exception to this is when using the 'Alternative' instance of
-- 'RequestData'. A 'RequestData' action can have multiple branches defined
-- with '(<|>)', and each branch can independently query the full request body.
-- This allows for such things as accepting multiple data formats in the request
-- body.
module Web.DumpTruck.RequestData where

import Web.DumpTruck.Capture
import Web.DumpTruck.Cookie
import Web.DumpTruck.Form

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Aeson (FromJSON, fromJSON, json')
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString, breakSubstring)
import Data.Map.Strict (Map)
import Data.Text.Encoding (decodeUtf8')
import Network.Wai
import Network.HTTP.Types

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M

-- Data sources:
-- Response body (gets consumed, alternative allows multiplexing)
-- Query parameters
-- Cookies
-- Request headers

-- | A 'RequestData' value is an action to query and parse the various sources
-- of data found in a request.
newtype RequestData a = RequestData { unRequestData ::
    Map ByteString (Maybe ByteString)
        -> Map ByteString ByteString
        -> Map HeaderName ByteString
        -> DataQuery a }

-- | A data type representing the result of a data query, which may either fail,
-- produce a value, or signal that it requires additional input in order to
-- continue processing.
data DataQuery a = QueryFail
                 | QueryDone a
                 | Incomplete (ByteString -> RequestData a)

-- By hiding the pattern matching behind a function we can build up thunks
-- instead of forcing evalutation early, allowing RequestData's Alternative
-- instance to only attempt to parse a response body in one way at a time
-- while also automatically caching input chunks.
feedInput :: RequestData a -> ByteString -> RequestData a
feedInput (RequestData r) b = RequestData go
  where
    go m1 m2 m3 = case r m1 m2 m3 of
        QueryFail -> QueryFail
        QueryDone a -> (QueryDone a)
        Incomplete f -> unRequestData (f b) m1 m2 m3

instance Functor RequestData where
    fmap f (RequestData r) = RequestData go
      where
        go m1 m2 m3 = case r m1 m2 m3 of
            QueryFail -> QueryFail
            QueryDone a -> QueryDone (f a)
            Incomplete g -> Incomplete (fmap f . g)

instance Applicative RequestData where
    pure a = RequestData (\_ _ _ -> QueryDone a)
    RequestData f <*> RequestData a = RequestData go
      where
        go m1 m2 m3 = case f m1 m2 m3 of
            QueryFail -> QueryFail
            QueryDone f' -> case a m1 m2 m3 of
                QueryFail -> QueryFail
                QueryDone a' -> QueryDone (f' a')
                Incomplete g -> Incomplete (fmap f' . g)
            Incomplete f' -> case a m1 m2 m3 of
                QueryFail -> QueryFail
                QueryDone a' -> Incomplete (fmap ($ a') . f')
                Incomplete g -> Incomplete (\b -> f' b <*> g b)

instance Alternative RequestData where
    empty = RequestData (\_ _ _ -> QueryFail)
    RequestData r1 <|> r2 = RequestData go
      where
        go m1 m2 m3 = case r1 m1 m2 m3 of
            QueryFail -> unRequestData r2 m1 m2 m3
            QueryDone a -> QueryDone a
            -- See the comments for 'feedInput' for why it is used
            Incomplete f -> Incomplete (\b -> f b <|> feedInput r2 b)

instance Monad RequestData where
    return = pure
    RequestData r >>= k = RequestData go
      where
        go m1 m2 m3 = case r m1 m2 m3 of
            QueryFail -> QueryFail
            QueryDone a -> unRequestData (k a) m1 m2 m3
            Incomplete f -> Incomplete (\b -> f b >>= k)

-- | Transforms a 'Parser' into a 'RequestData' action. This action will feed
-- chunks from the request body into parser, either succeeding or failing
-- depending on whether the 'Parser' succeeds or fails.
parserToReqData :: Parser a -> RequestData a
parserToReqData p = RequestData go
  where
    go _ _ _ = Incomplete (resultToReqData . parse p)
    resultToReqData :: Result a -> RequestData a
    resultToReqData res = case res of
        Fail _ _ _ -> empty
        Done _ a -> pure a
        Partial c -> RequestData (\_ _ _ -> Incomplete (resultToReqData . c))

-- | Given a 'Request', runs a 'RequestData' action. If the 'RequestData' action
-- fails then it will return 'Nothing', otherwise it will produce a value of the
-- appropriate type.
runRequestData :: Request -> RequestData a -> IO (Maybe a)
runRequestData req = go
  where
    m1 = M.fromList (queryString req)
    m2 = M.fromList (getCookies req)
    m3 = M.fromList (requestHeaders req)
    go (RequestData r) = case r m1 m2 m3 of
        QueryFail -> return Nothing
        QueryDone a -> return (Just a)
        Incomplete f -> do
            input <- requestBody req
            go (f input)

-- | Attempts to retrieve the request body as HTML form data, producing a list
-- of key-value pairs for each item in the form data.
getReqBodyAsForm :: RequestData [(ByteString, ByteString)]
getReqBodyAsForm = parserToReqData formParser

-- | Attempts to retrieve the request body as JSON data, then attempts to
-- transform that JSON data into a value of the appropriate type.
getReqBodyAsJson :: FromJSON a => RequestData a
getReqBodyAsJson = parserToReqData json' >>= go . fromJSON
  where
    go (A.Error _) = empty
    go (A.Success a) = return a

-- | Attempts to retrieve the value corresponding to the given key for the
-- given map.
getFromMap :: Ord a => Map a b -> a -> RequestData b
getFromMap m a = case M.lookup a m of
    Just b -> pure b
    Nothing -> empty

getFromMap' :: Ord a => Map a b -> a -> DataQuery b
getFromMap' m a = case M.lookup a m of
    Just b -> QueryDone b
    Nothing -> QueryFail

-- | Attempts to retrieve the query parameter with the given key name. Query
-- parameters are key-value pairs found at the end of a request URI after a @?@
-- character.
--
-- Query parameters do not necessarily have a corresponding value for a given
-- key, and thus are represented as a 'Maybe' value. To instead use an empty
-- 'ByteString' to signify no value and thus avoiding the 'Maybe', use
-- 'getQueryParam'' instead.
getQueryParam :: ByteString -> RequestData (Maybe ByteString)
getQueryParam b = RequestData go
  where
    go m _ _ = getFromMap' m b

-- | Attempts to retrieve the query parameter with the given key name. Query
-- parameters are key-value pairs found at the end of a request URI after a @?@
-- character.
--
-- Query parameters do not necessarily have a corresponding value for a given
-- key, and in this case the value is simply an empty 'ByteString'. To instead
-- use a 'Maybe' value to signify no value, use 'getQueryParam' instead.
getQueryParam' :: ByteString -> RequestData ByteString
getQueryParam' = fmap (maybe "" id) . getQueryParam

-- | Attempts to retrieve the cookie value with the given name.
getCookie :: ByteString -> RequestData ByteString
getCookie b = RequestData go
  where
    go _ m _ = getFromMap' m b

-- | Attempts to retrieve the header value with the given header name.
getHeader :: HeaderName -> RequestData ByteString
getHeader b = RequestData go
  where
    go _ _ m = getFromMap' m b

-- | Masks a 'Maybe' value in a 'RequestData' action, failing if the action
-- produces a 'Nothing' value and producing the value otherwise.
withReqMaybe :: RequestData (Maybe a) -> RequestData a
withReqMaybe r = r >>= go
  where
    go (Just a) = pure a
    go Nothing = empty

-- | Masks an 'Either' value in a 'RequestData' action. If the action produces a
-- 'Left' value then that value is ignored and the action fails. If the action
-- produces a 'Right' value then the 'RequestData' action produces that value.
withReqEither :: RequestData (Either a b) -> RequestData b
withReqEither r = r >>= go
  where
    go (Left _) = empty
    go (Right b) = pure b

-- | Guards on the given 'RequestData' action producing a 'ByteString' that
-- contains the given 'ByteString'. If the action fails or produces a
-- 'ByteString' that does not contain the given 'ByteString', the resulting
-- action fails.
containingString :: ByteString -> RequestData ByteString
                 -> RequestData ByteString
containingString t rd = do
    input <- rd
    let (_,c) = breakSubstring t input
    if B.null c
    then empty
    else return input

-- | Takes a 'RequestData' action that produces a 'ByteString' and attempts to
-- parse it into the appropriate data type through its 'Captureable' instance.
asCaptureable :: Captureable a => RequestData ByteString -> RequestData a
asCaptureable = withReqMaybe . fmap performCapture . withReqEither
    . fmap decodeUtf8'
