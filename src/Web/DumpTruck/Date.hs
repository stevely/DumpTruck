{-
 - Date.hs
 - By Steven Smith
 -}

{-# LANGUAGE OverloadedStrings #-}

-- | Contains functions for handling the GMT date/time formats that HTTP uses.
-- Note that while the 'GmtTime' type is just a wrapper around 'UTCTime', GMT
-- time and UTC time have some subtle differences. This code attempts to do the
-- sensible thing for handling e.g. leap seconds, but for just comparing
-- timestamps there shouldn't be any unfortunate surprises.
module Web.DumpTruck.Date
( GmtTime(..)
, gmtToByteString
, parseGmtTime
, gmtTimeParser
)
where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.Monoid
import Data.String (fromString)
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Clock

import qualified Data.ByteString as B

-- | GMT time, represented as a wrapper over 'UTCTime'. There are some subtle
-- differences between the two, but they are roughly the same.
newtype GmtTime = GmtTime { toUtcTime :: UTCTime }
    deriving (Eq, Ord)

-- From UTCTime to GMT

-- | Converts a 'GmtTime' timestamp into a strict 'ByteString' containing the
-- RFC 1123 representation of the timestamp.
gmtToByteString :: GmtTime -> ByteString
gmtToByteString (GmtTime t) =
    B.concat [ weekDayName wd
             , ", "
             , fromString (to2d d)
             , " "
             , monthName mon
             , " "
             , fromString (show y)
             , " "
             , fromString (to2d h)
             , ":"
             , fromString (to2d min)
             , ":"
             , fromString (to2d s)
             , " GMT" ]
  where
    day = utctDay t
    time = utctDayTime t
    (y,mon,d) = toGregorian day
    (_,_,wd) = toWeekDate day
    (h,min,s) = splitTime time
    to2d = toTwoDigit . show

toTwoDigit :: String -> String
toTwoDigit x@[_] = '0':x
toTwoDigit s = s

splitTime :: DiffTime -> (Int, Int, Int)
splitTime t = (t' `div` (60*60), (t' `div` 60) `mod` 60, t' `mod` 60)
  where
    t' = min 86399 (floor t) -- Blatantly ignore leap seconds

monthName :: Int -> ByteString
monthName  1 = "Jan"
monthName  2 = "Feb"
monthName  3 = "Mar"
monthName  4 = "Apr"
monthName  5 = "May"
monthName  6 = "Jun"
monthName  7 = "Jul"
monthName  8 = "Aug"
monthName  9 = "Sep"
monthName 10 = "Oct"
monthName 11 = "Nov"
monthName 12 = "Dec"

weekDayName :: Int -> ByteString
weekDayName 1 = "Mon"
weekDayName 2 = "Tue"
weekDayName 3 = "Wed"
weekDayName 4 = "Thu"
weekDayName 5 = "Fri"
weekDayName 6 = "Sat"
weekDayName 7 = "Sun"

-- From GMT to UTCTime

-- | Attempts to convert the given strict 'ByteString' containing an HTTP date
-- into its corresponding 'UTCTime' value, returning 'Nothing' on parse
-- failure. The date formats accepted are those defined by RFC 1123, RFC 850,
-- and ANSI C's @asctime()@ format. (All these formats are required for HTTP/1.1
-- compliance. See RFC 2616, sec. 3.3.1)
parseGmtTime :: ByteString -> Maybe GmtTime
parseGmtTime = go . parseOnly httpDate
  where
    go (Left _) = Nothing
    go (Right d) = fmap GmtTime (httpDateToUTCTime d)

-- Ignoring day-of-week, because don't care
data HttpDate = HttpDate {
    httpDateYear :: Integer,
    httpDateMonth :: Int,
    httpDateDay :: Int,
    httpDateHour :: Int,
    httpDateMinute :: Int,
    httpDateSecond :: Int
}
    deriving (Show, Read)

-- | A parser for GMT time. This parser accepts all the formats required for
-- HTTP/1.1 compliance.
gmtTimeParser :: Parser GmtTime
gmtTimeParser = fmap httpDateToUTCTime httpDate >>= maybe empty (pure . GmtTime)

httpDateToUTCTime :: HttpDate -> Maybe UTCTime
httpDateToUTCTime (HttpDate y mon d h min s) =
    UTCTime <$> (fromGregorianValid y mon d) <*> pure (fromIntegral $ h*60*60 + min*60 + s)

httpDate :: Parser HttpDate
httpDate = rfc1123_date <|> rfc850_date <|> asctime_date

rfc1123_date :: Parser HttpDate
rfc1123_date = wkday *> string ", " *> datetime <* string " GMT"
  where
    datetime = mkDate
           <$> twoDigit
           <*> (string " " *> month)
           <*> (string " " *> fourDigit)
           <*> (string " " *> twoDigit)
           <*> (string ":" *> twoDigit)
           <*> (string ":" *> twoDigit)
    mkDate d mon y h min s = HttpDate y mon d h min s

rfc850_date :: Parser HttpDate
rfc850_date = weekday *> string ", " *> datetime <* string " GMT"
  where
    datetime = mkDate
           <$> twoDigit
           <*> (string "-" *> month)
           <*> (y2k <$> (string "-" *> twoDigit))
           <*> (string " " *> twoDigit)
           <*> (string ":" *> twoDigit)
           <*> (string ":" *> twoDigit)
    mkDate d mon y h min s = HttpDate y mon d h min s
    y2k y | y < 50    = y + 2000 -- Remember y2k?
          | otherwise = y + 1900 -- Good times

asctime_date = wkday *> string " " *> datetime
  where
    datetime = mkDate
           <$> (month <* string " ")
           <*> ((string " " *> oneDigit) <|> twoDigit)
           <*> (string " " *> twoDigit)
           <*> (string ":" *> twoDigit)
           <*> (string ":" *> twoDigit)
           <*> (string " " *> fourDigit)
    mkDate mon d h min s y = HttpDate y mon d h min s

wkday :: Parser B.ByteString
wkday = string "Mon"
    <|> string "Tue"
    <|> string "Wed"
    <|> string "Thu"
    <|> string "Fri"
    <|> string "Sat"
    <|> string "Sun"

weekday :: Parser B.ByteString
weekday = string "Monday"
      <|> string "Tuesday"
      <|> string "Wednesday"
      <|> string "Thursday"
      <|> string "Friday"
      <|> string "Saturday"
      <|> string "Sunday"

month :: Parser Int
month = choice $ fmap go pairs
  where
    go (s,d) = string s *> pure d
    pairs = [ ("Jan", 1)
            , ("Feb", 2)
            , ("Mar", 3)
            , ("Apr", 4)
            , ("May", 5)
            , ("Jun", 6)
            , ("Jul", 7)
            , ("Aug", 8)
            , ("Sep", 9)
            , ("Oct", 10)
            , ("Nov", 11)
            , ("Dec", 12) ]

oneDigit :: Read n => Parser n
oneDigit = read . (:[]) <$> digit

twoDigit :: Read n => Parser n
twoDigit = read <$> count 2 digit

fourDigit :: Read n => Parser n
fourDigit = read <$> count 4 digit
