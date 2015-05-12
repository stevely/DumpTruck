{-
 - Capture.hs
 - By Steven Smith
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- | 'Captureable' is a type class for types that can be parsed from 'Text'.
-- It is used by the 'capture' 'Route' to automatically handle parsing.
--
-- If you would like to make your own data type 'Captureable', then all that
-- needs to be implemented is 'captureParser', which is a 'Parser' from
-- <http://hackage.haskell.org/package/attoparsec attoparsec>. Do keep in mind
-- when writing 'Captureable' instances that the 'Text' input will be from a
-- @URI@ path segment, so not all possible characters will be available.
module Web.DumpTruck.Capture where

import Control.Applicative
import Data.Text (Text)
import Data.Text.Encoding
import Data.Attoparsec.Text
import Data.ByteString.Builder

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

-- | A type is 'Captureable' if a 'Parser' exists for it that can parse 'Text'
-- input coming from a @URI@ path segment.
-- 
-- Minimum definition: 'captureParser'
class Captureable a where
    -- | The @URI@ path segment 'Parser' for this data type
    captureParser :: Parser a
    -- | Performs the 'Text' parsing, which may fail.
    --
    -- Note: The default implementation should cover almost all use-cases for
    -- normal data types. This method exists solely for cases where parsing
    -- cannot fail, specifically for converting between various string types.
    performCapture :: Text -> Maybe a
    performCapture = either (const Nothing) Just . parseOnly captureParser

instance Captureable Text where
    captureParser = takeText
    performCapture = Just

instance Captureable LT.Text where
    captureParser = takeLazyText
    performCapture = Just . LT.fromStrict

instance Captureable BS.ByteString where
    captureParser = fmap encodeUtf8 takeText
    performCapture = Just . encodeUtf8

instance Captureable LBS.ByteString where
    captureParser = fmap (toLazyByteString . encodeUtf8Builder) takeText
    performCapture = Just . toLazyByteString . encodeUtf8Builder

instance Captureable Int where
    captureParser = decimal

instance Captureable Double where
    captureParser = double

instance Captureable Bool where
    captureParser = string "true" *> pure True <|> string "false" *> pure False

instance Captureable Char where
    captureParser = anyChar

-- String
instance Captureable [Char] where
    captureParser = fmap T.unpack takeText
    performCapture = Just . T.unpack

-- Note that stacking lists [[a]] is unlikely to do anything useful
instance Captureable a => Captureable [a] where
    captureParser = captureParser `sepBy1` (char ',')
