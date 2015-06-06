{-
 - DumpTruckTest.hs
 - By Steven Smith
 -}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.DumpTruck
import Web.DumpTruck.Cookie
import Web.DumpTruck.RequestData

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.Attoparsec.Text
import Data.ByteString.Lazy (ByteString)
import Data.IORef
import Data.List (intersperse)
import Data.Maybe
import Data.String (fromString)
import Data.Text (Text)
import Network.Wai
import Network.Wai.Handler.Warp hiding (withManager)
import Data.Map (fromList)

import qualified Data.Text as T

-- Main routes

data JsonFormTestData = JsonFormTestData Text Int

instance FromJSON JsonFormTestData where
    parseJSON (Object v) = JsonFormTestData <$> v .: "textfield" <*> v .: "hiddenfield"
    parseJSON _ = mzero

app :: Route s ()
app = do
    directory "assets"
    route "route" $ do
        route "test" $ do
            routeEnd . get $ raw "Test end"
            route "one" . get $ raw "Test one"
        route "othertest" $ do
            route "two" . get $ raw "Test two"
    route "captest" $ do
        captureInt $ \n -> get (json n)
        capture $ \s -> get (json (s :: Text))
    route "json" . get . json $ object ["message" .= ("Hello, World!" :: Text)]
    route "plaintext" . get $ do
        addHeader ("Content-Type", "text/plain")
        raw "Hello, World!"
    route "form" $ do
        post $ do
            (t,h) <- withRequestData $ do
                    form <- getReqBodyAsFormMap
                    textfield <- asCaptureable (getFromMap form "textfield")
                    hiddenfield <- asCaptureable (getFromMap form "hiddenfield")
                    return (textfield :: Text, hiddenfield :: Int)
                <|> do
                    JsonFormTestData t h <- getReqBodyAsJson
                    return (t,h)
                <|> do
                    input <- asCaptureable getReqBodyRaw -- Fallback for debugging
                    return (input, -1)
            json $ object ["textfield" .= t, "hiddenfield" .= h]
        get $ do
            (t,h) <- withRequestData $ do
                textfield <- asCaptureable (getQueryParam' "textfield")
                hiddenfield <- asCaptureable (getQueryParam' "hiddenfield")
                return (textfield :: Text, hiddenfield :: Int)
            json $ object ["textfield" .= t, "hiddenfield" .= h]
    route "cookie" $ do
        routeEnd . get $ do
            c <- withRequestData $ do
                cookie <- optional (asCaptureable (getCookie "testcookie"))
                return (maybe (0 :: Int) (+1) cookie)
            addCookie $ mkCookie ("testcookie", fromString (show c))
                      $ setCookieDomain "localhost"
                      . setCookiePath "/"
            addHeader ("Cache-Control", "no-cache")
            file "assets/cookietests.html"
        route "delete" . get $ do
            addHeader ("Cache-Control", "no-cache")
            addCookie $ deleteCookie "testCookie"
                      $ setCookieDomain "localhost"
                      . setCookiePath "/"
            file "assets/cookietests.html"
        route "raw" . get $ do
            c <- withRequestData $ do
                cookie <- optional (asCaptureable (getHeader "Cookie"))
                return (maybe "No cookies" id cookie)
            raw c

-- App startup

main :: IO ()
main = do
    run 8000 (mkDumpTruckApp' app)
