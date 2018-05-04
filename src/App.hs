{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App where

import Network.Wai
import Servant

type KeyValueApi =
  "set" :> Get '[JSON] ()

keyValueApi :: Proxy KeyValueApi
keyValueApi = Proxy

mkApp :: IO Application
mkApp = return $ serve keyValueApi server

server :: Handler ()
server = return ()
