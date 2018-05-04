{-# LANGUAGE OverloadedStrings #-}

module AppSpec where

import App
import Data.ByteString.Lazy
import qualified Data.ByteString as Sbs
import Network.HTTP.Client
import Network.HTTP.Types
import Network.Wai.Handler.Warp
import Test.Hspec

withApp :: (String -> IO a) -> IO a
withApp test = testWithApplication mkApp $ \ port ->
  test ("http://localhost:" ++ show port)

request :: Sbs.ByteString -> String -> IO (Response ByteString)
request method url = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest url
  httpLbs (req{ method = method }) manager

spec = describe "app" $ do
  it "allows to set a key" $ do
    withApp $ \ baseUrl -> do
      r <- request "POST" (baseUrl ++ "/set?foo=bar")
      responseStatus r `shouldBe` ok200

  it "allows to fetch a set key" $ do
    withApp $ \ baseUrl -> do
      request "POST" (baseUrl ++ "/set?foo=bar")
      r <- request "GET" (baseUrl ++ "/get?key=foo")
      responseStatus r `shouldBe` ok200
      responseBody r `shouldBe` "bar"

  it "returns a 404 for a missing key" $ do
    withApp $ \ baseUrl -> do
      r <- request "GET" (baseUrl ++ "/get?key=foo")
      responseStatus r `shouldBe` notFound404
