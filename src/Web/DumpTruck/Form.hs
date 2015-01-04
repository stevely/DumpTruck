{-
 - Form.hs
 - By Steven Smith
 -}

{-# LANGUAGE OverloadedStrings #-}

-- | Form data parsing, usually as a result of HTML @form@s being submitted
-- through @POST@.
--
-- Note that 'parseFormData' won't work if the request is made by an HTML @form@
-- with its method set to @GET@, as the form data will be stored in the query
-- string of the URI instead of the request body. To get the form data from such
-- a form, either change the @form@'s method to @POST@ and use 'parseFormData'
-- or use 'queryString' from 'Network.Wai'.
module Web.DumpTruck.Form
( parseFormData
, formParser
, urlencoded
, parseFormJson
)
where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Attoparsec.ByteString
import Data.Bits
import Data.ByteString (ByteString)
import Network.Wai

import qualified Data.ByteString as B

-- | Attempts to parse the 'Request' body as form data with the media type
-- \"@application\/x-www-form-urlencoded@\". Returns a list of key-value pairs
-- if successful. This will eagerly consume the 'Request' body in its entirety
-- if the parse is successful, and will consume at least one chunk in the case
-- of parse failure.
parseFormData :: Request -> IO (Maybe [(ByteString, ByteString)])
parseFormData req = go (parse formParser)
  where
    go f = do
        input <- requestBody req
        go' (f input)
    go' (Fail _ _ _) = return Nothing
    go' (Done _ r) = return (Just r)
    go' (Partial c) = go c

-- | Attempts to parse the given 'ByteString' as form data with the media type
-- \"@application\/x-www-form-urlencoded@\". Returns a list of key-value pairs
-- if successful.
parseFormDataBS :: ByteString -> Maybe [(ByteString, ByteString)]
parseFormDataBS s = toMaybe (parseOnly formParser s)
  where
    toMaybe (Left _) = Nothing
    toMaybe (Right r) = Just r

-- | A 'Parser' for URL encoded form data.
formParser :: Parser [(ByteString, ByteString)]
formParser = sepBy1 keyval (string "&")
  where
    keyval = (,) <$> urlencoded <*> (string "=" *> urlencoded)

-- | A 'Parser' for a URL encoded form key or value.
urlencoded :: Parser ByteString
urlencoded = B.concat <$> many1 (escaped <|> plusSpace <|> regular)
  where
    escaped = string "%" *> (hexToWord8 <$> hexDigit <*> hexDigit)
    plusSpace = const " " <$> word8 0x2B
    regular = takeWhile1 isAlphaNum
    hexToWord8 a b = B.singleton (shift a 4 .|. b)
    hexDigit = satisfyWith nibbleConv (/= 0x10)
    nibbleConv n
        | n >= 0x30 && n <= 0x39 = n - 0x30
        | n >= 0x41 && n <= 0x46 = n - 0x41 + 10
        | n >= 0x61 && n <= 0x66 = n - 0x61 + 10
        | otherwise = 0x10
    isAlphaNum n = (n >= 0x30 && n <= 0x39)
                || (n >= 0x41 && n <= 0x5A)
                || (n >= 0x61 && n <= 0x7A)

parseFormJson :: FromJSON a => Request -> IO (Maybe a)
parseFormJson req = go (parse json')
  where
    go f = do
        input <- requestBody req
        go' (f input)
    go' (Fail _ _ _) = return Nothing
    go' (Done _ r) = return (parseMaybe parseJSON r)
    go' (Partial c) = go c
