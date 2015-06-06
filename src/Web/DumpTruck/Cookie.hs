{-
 - Cookie.hs
 - By Steven Smith
 -}

{-# LANGUAGE OverloadedStrings #-}

-- | Creation, modification, and encoding of HTTP 'Cookie's.
--
-- 'Cookie' creation is designed to be done in a builder style:
--
-- @
-- myCookie :: Cookie
-- myCookie = mkCookie (\"AreCookiesDelicious\", \"Yes\")
--          $ setCookieDomain \"example.com\"
--          . setCookiePath "\/foo\/bar\/baz"
--          . setCookieSecure
--          . setCookieHttpOnly
-- @
--
-- A <https://hackage.haskell.org/package/lens lens> API is provided as well
-- for advanced users. DumpTruck itself does not use any lens library, thus the
-- lenses are hand written to avoid unnecessary dependencies.
module Web.DumpTruck.Cookie
( Cookie (..)
, CookieExpiration (..)
, mkCookie
, setCookieDomain
, setCookiePath
, setCookieExpires
, setCookieMaxAge
, setCookieSecure
, setCookieHttpOnly
, deleteCookie
, cookieValue
, cookieDomain
, cookiePath
, cookieExpires
, cookieSecure
, cookieHttpOnly
, encodeCookie
, parseCookies
, cookieParser
, getCookies
)
where

import Web.DumpTruck.Date

import Control.Applicative
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.String
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Data.Time.Calendar (fromGregorian)
import Network.HTTP.Types
import Network.Wai

import qualified Data.ByteString as B

-- | Cookie datatype. It's recommended that users don't reference fields
-- directly, instead opting to use the provided builder functions.
data Cookie = Cookie {
    _cookieValue :: (ByteString, ByteString),
    _cookieDomain :: Maybe ByteString,
    _cookiePath :: Maybe ByteString,
    _cookieExpires :: Maybe CookieExpiration,
    _cookieSecure :: Bool,
    _cookieHttpOnly :: Bool
}

-- | Simple datatype for the two methods to handle cookie expiration. 'MaxAge'
-- is in seconds.
data CookieExpiration = Expires GmtTime
                      | MaxAge Int

-------------------------------------------------------------------------------
-- Building Cookies
-------------------------------------------------------------------------------

-- | Produces a 'Cookie' given a key-value pair and a builder function. Extra
-- fields are not included by default. As an example:
--
-- @
-- myCookie :: Cookie
-- myCookie = mkCookie (\"CookieKey\", \"CookieValue\")
--          $ setCookieDomain \"example.com\"
--          . setCookieHttpOnly
-- @
mkCookie :: (ByteString, ByteString) -> (Cookie -> Cookie) -> Cookie
mkCookie kv f = f $ Cookie kv Nothing Nothing Nothing False False

-- | Produces a 'Cookie' given a key name and a builder function that is marked
-- as needing to be deleted by the client. The server still needs to encode and
-- send this cookie to the client in order for the deletion to actually happen.
--
-- The cookie key, domain, and path must all be equal to the original 'Cookie'
-- sent to the client for the 'Cookie' deletion to happen successfully, so
-- ensure that the domain and path are present if they were originally.
deleteCookie :: ByteString -> (Cookie -> Cookie) -> Cookie
deleteCookie b f = mkCookie (b, "") (f . setCookieExpires t)
  where
    -- We need to set the expiration time to some arbitrary point in the past.
    -- For obvious reasons, we use the North American release date for the
    -- SNES game Final Fantasy VI (titled Final Fantasy III for its North
    -- American release)
    t = GmtTime (UTCTime (fromGregorian 1994 10 20) (secondsToDiffTime 0))

-- | Builder function that sets the 'Cookie' domain. This controls what domains
-- the client will send this cookie to. For example, if the domain is set to
-- \"example.com\" the client will send the cookie to \"www.example.com\" and to
-- \"other.example.com\". If the domain is omitted the client will only send the
-- cookie to the origin server.
setCookieDomain :: ByteString -> Cookie -> Cookie
setCookieDomain d c = c { _cookieDomain = Just d }

-- | Builder function that sets the 'Cookie' path. This restricts the cookie to
-- only being sent from the client if the URI path starts with the given path.
-- For example, given a path of \"\/foo\" the client will send the cookie when
-- retrieving \"\/foo\" and \"\/foo\/bar\", but not \"\/bar\".
setCookiePath :: ByteString -> Cookie -> Cookie
setCookiePath p c = c { _cookiePath = Just p }

-- | Builder function that sets the 'Cookie' to expire at the given time. It's
-- generally recommended that this be some time in the future. To force a cookie
-- to expire use 'deleteCookie'.
setCookieExpires :: GmtTime -> Cookie -> Cookie
setCookieExpires t c = c { _cookieExpires = Just (Expires t) }

-- | Builder function that sets the 'Cookie' to expire the given number of
-- seconds into the future. This should be greater than zero. To force a cookie
-- to expire use 'deleteCookie'.
setCookieMaxAge :: Int -> Cookie -> Cookie
setCookieMaxAge a c = c { _cookieExpires = Just (MaxAge a) }

-- | Builder function that marks the 'Cookie' as secure. Secure cookies are only
-- sent by the client when it is using a secure channel (typically SSL). If the
-- client is making a request over plain HTTP it will not send the cookie if it
-- is marked as secure. 'Cookie's built through 'mkCookie' are not secure by
-- default.
setCookieSecure :: Cookie -> Cookie
setCookieSecure c = c { _cookieSecure = True }

-- | Builder function that marks the 'Cookie' as HTTP-only. HTTP-only cookies
-- will only be sent by the client over HTTP, and generally will only be made
-- available through HTTP. Most notably, this means that browsers will not make
-- the cookie available through Javascript scripts. 'Cookie's built through
-- 'mkCookie' are not HTTP-only by default.
setCookieHttpOnly :: Cookie -> Cookie
setCookieHttpOnly c = c { _cookieHttpOnly = True }

-------------------------------------------------------------------------------
-- Building Cookies
-------------------------------------------------------------------------------

-- | Encodes the given 'Cookie' into a 'Header'.
encodeCookie :: Cookie -> Header
encodeCookie c = ("Set-Cookie", B.concat $ concat [v, d, p, e, s, h])
  where
    v = case _cookieValue c of
             (k,v) -> [k, "=", v]
    d = case _cookieDomain c of
             Nothing -> []
             Just d' -> ["; Domain=", d']
    p = case _cookiePath c of
             Nothing -> []
             Just p' -> ["; Path=", p']
    e = case _cookieExpires c of
             Nothing -> []
             Just (Expires e') -> ["; Expires=", gmtToByteString e']
             Just (MaxAge e') -> ["; Max-Age=", fromString (show e')]
    s = case _cookieSecure c of
             False -> []
             True -> ["; Secure"]
    h = case _cookieHttpOnly c of
             False -> []
             True -> ["; HttpOnly"]

-- | Retrieves the cookies from a given 'Request' as a list of key-value pairs.
getCookies :: Request -> [(ByteString, ByteString)]
getCookies = go . lookup "Cookie" . requestHeaders
  where
    go v = case v >>= parseCookies of
                Nothing -> []
                (Just cs) -> cs

-- | Attempts to parse the given 'ByteString' as a cookie string sent by a
-- client. If successful, will return a list of key-value pairs of all the
-- contained cookies.
parseCookies :: ByteString -> Maybe [(ByteString, ByteString)]
parseCookies = go . parseOnly cookieParser
  where
    go (Left _) = Nothing
    go (Right c) = Just c

-- | A 'Parser' for HTTP cookies as sent by the client in the \"@Cookie@\"
-- header.
cookieParser :: Parser [(ByteString, ByteString)]
cookieParser = sepBy1 cookie (string "; ")
  where
    cookie = (,) <$> takeWhile1 token <*> (string "=" *> cookieValue)
    cookieValue = takeWhile1 octet
              <|> (string "\"" *> (takeWhile1 octet <* string "\""))
    -- See RFC 2616 sec. 2.2 for token definition
    -- 0x21-0x7F, skipping 22, 28-29, 2C, 2F, 3A-40, 5B-5D, 7B, 7D
    token n = n == 0x21
          || (n >= 0x23 && n <= 0x27)
          ||  n == 0x2A
          ||  n == 0x2B
          ||  n == 0x2D
          ||  n == 0x2E
          || (n >= 0x30 && n <= 0x39)
          || (n >= 0x41 && n <= 0x5A)
          || (n >= 0x5E && n <= 0x7A)
          ||  n == 0x7C
          ||  n == 0x7E
    -- See RFC 6265 sec. 4.1.1 for cookie-octet definition
    octet n = n == 0x21
          || (n >= 0x23 && n <= 0x2B)
          || (n >= 0x2D && n <= 0x3A)
          || (n >= 0x3C && n <= 0x5B)
          || (n >= 0x5D && n <= 0x7E)

-- Cookie lenses (hand-written to avoid lens dependency)

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

-- | Lens for the key-value pair payload for a 'Cookie'.
--
-- If you are using <https://hackage.haskell.org/package/lens lens> then this
-- has the following type:
--
-- @
-- cookieValue :: Lens' Cookie (ByteString, ByteString)
-- @
cookieValue :: Functor f => ((ByteString, ByteString) -> f (ByteString, ByteString))
            -> Cookie -> f Cookie
cookieValue f c = fmap (\v -> c { _cookieValue = v }) (f (_cookieValue c))

-- | Lens for the domain of a 'Cookie'.
--
-- If you are using <https://hackage.haskell.org/package/lens lens> then this
-- has the following type:
--
-- @
-- cookieDomain :: Lens' Cookie (Maybe ByteString)
-- @
cookieDomain :: Functor f => (Maybe ByteString -> f (Maybe ByteString))
             -> Cookie -> f Cookie
cookieDomain f c = fmap (\d -> c { _cookieDomain = d }) (f (_cookieDomain c))

-- | Lens for the path of a 'Cookie'.
--
-- If you are using <https://hackage.haskell.org/package/lens lens> then this
-- has the following type:
--
-- @
-- cookiePath :: Lens' Cookie (Maybe ByteString)
-- @
cookiePath :: Functor f => (Maybe ByteString -> f (Maybe ByteString))
           -> Cookie -> f Cookie
cookiePath f c = fmap (\p -> c { _cookiePath = p }) (f (_cookiePath c))

-- | Lens for the expiration of a 'Cookie'.
--
-- If you are using <https://hackage.haskell.org/package/lens lens> then this
-- has the following type:
--
-- @
-- cookieExpires :: Lens' Cookie (Maybe CookieExpiration)
-- @
cookieExpires :: Functor f => (Maybe CookieExpiration -> f (Maybe CookieExpiration))
              -> Cookie -> f Cookie
cookieExpires f c = fmap (\e -> c { _cookieExpires = e }) (f (_cookieExpires c))

-- | Lens for the secure flag of a 'Cookie'.
--
-- If you are using <https://hackage.haskell.org/package/lens lens> then this
-- has the following type:
--
-- @
-- cookieSecure :: Lens' Cookie Bool
-- @
cookieSecure :: Functor f => (Bool -> f Bool) -> Cookie -> f Cookie
cookieSecure f c = fmap (\s -> c { _cookieSecure = s }) (f (_cookieSecure c))

-- | Lens for the HTTP-only flag of a 'Cookie'.
--
-- If you are using <https://hackage.haskell.org/package/lens lens> then this
-- has the following type:
--
-- @
-- cookieHttpOnly :: Lens' Cookie Bool
-- @
cookieHttpOnly :: Functor f => (Bool -> f Bool) -> Cookie -> f Cookie
cookieHttpOnly f c = fmap (\h -> c { _cookieHttpOnly = h }) (f (_cookieHttpOnly c))
