{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module App where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Map
import Data.String.Conversions
import Data.Text (Text)
import Network.Wai
import Servant
import Servant.Server.Internal.Router
import Servant.Server.Internal.RoutingApplication

type KeyValueApi =
  "set" :> DynamicQueryParam :> Post '[PlainText] NoContent :<|>
  "get" :> QueryParam "key" Text :> Get '[OctetStream] ByteString

keyValueApi :: Proxy KeyValueApi
keyValueApi = Proxy

type Store = Map ByteString ByteString

mkApp :: IO Application
mkApp = do
  storeRef <- newMVar empty
  return $ serve keyValueApi (server storeRef)

server :: MVar Store -> Server KeyValueApi
server storeRef = set storeRef :<|> get storeRef

set :: MVar Store -> (ByteString, ByteString) -> Handler NoContent
set storeRef (key, value) = liftIO $ do
  modifyMVar_ storeRef $ \ store -> do
    return (insert key value store)
  return NoContent

get :: MVar Store -> Maybe Text -> Handler ByteString
get storeRef mKey = case mKey of
  Nothing -> throwError err400
  Just key -> do
    store <- liftIO $ readMVar storeRef
    case Data.Map.lookup (cs key) store of
      Just value -> return value
      Nothing -> throwError err404

-- * servant combinator for dynamic query params:

data DynamicQueryParam

instance forall subApi context . HasServer subApi context =>
  HasServer (DynamicQueryParam :> subApi) context where

  type ServerT (DynamicQueryParam :> subApi) m =
    (ByteString, ByteString) -> ServerT subApi m

  hoistServerWithContext Proxy contextProxy natTransformation subServer =
    \ queryParams ->
      hoistServerWithContext
        (Proxy :: Proxy subApi)
        contextProxy
        natTransformation
        (subServer queryParams)

  route :: forall env . Proxy (DynamicQueryParam :> subApi)
    -> Context context
    -> Delayed env ((ByteString, ByteString) -> Server subApi)
    -> Router env
  route Proxy context subServer =
    let parseRequest :: Request -> DelayedIO (ByteString, ByteString)
        parseRequest req = case queryString req of
          (key, Just value) : _ -> return (key, value)
          _ -> delayedFailFatal err400{
            errBody = cs "query parameter missing"
          }
        delayed :: Delayed env (Server subApi)
        delayed = addParameterCheck subServer $ withRequest parseRequest
    in route (Proxy :: Proxy subApi) context delayed
