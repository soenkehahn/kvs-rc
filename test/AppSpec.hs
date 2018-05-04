
module AppSpec where

import App
import Data.ByteString.Lazy
import Network.HTTP.Client
import Network.HTTP.Types
import Network.Wai.Handler.Warp
import Test.Hspec

withApp :: (String -> IO a) -> IO a
withApp test = testWithApplication mkApp $ \ port ->
  test ("http://localhost:" ++ show port)

request :: String -> IO (Response ByteString)
request url = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest url
  httpLbs request manager

spec = describe "app" $ do
  it "allows to set a key" $ do
    withApp $ \ baseUrl -> do
      r <- request (baseUrl ++ "/set?foo=bar")
      responseStatus r `shouldBe` ok200
