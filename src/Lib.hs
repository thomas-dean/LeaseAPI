{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import           Data.Aeson
import           GHC.Generics
import           Network.Wai.Handler.Warp hiding (run)
import           Servant
import           System.IO

data Lease = Lease
  { leaseId          :: Integer
  , leaseDescription :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Lease

instance FromJSON Lease

exampleLease = Lease 0 "First Lease!"

type LeaseApi = "lease" :> Get '[ JSON] [Lease] :<|> "lease" :> Capture "leaseId" Integer :> Get '[ JSON] Lease

leaseApi :: Proxy LeaseApi
leaseApi = Proxy

run = do
  let port = 3000
      settings = setPort port $ setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) defaultSettings
  runSettings settings =<< mkApp

mkApp = return $ serve leaseApi server

server = getLeases :<|> getLeaseById

getLeases :: Handler [Lease]
getLeases = return [exampleLease]

getLeaseById :: Integer -> Handler Lease
getLeaseById =
  \case
    0 -> return exampleLease
    _ -> throwError err404
