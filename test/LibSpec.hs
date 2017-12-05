module LibSpec where

import           Control.Exception         (throwIO)
import           Lib                       hiding (getLeaseById, getLeases)
import           Network.HTTP.Client       (Manager, defaultManagerSettings,
                                            newManager)
import           Network.HTTP.Types.Status
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           Test.Hspec

getLeases :: ClientM [Lease]
getLease :: Integer -> ClientM Lease
getLeases :<|> getLease = client leaseApi

spec :: Spec
spec =
  describe "/lease" $
  withClient mkApp $ do
    it "lists an example lease" $ \env -> try env getLeases `shouldReturn` [Lease 0 "First Lease!"]
    it "allows to show lease by id" $ \env -> try env (getLease 0) `shouldReturn` Lease 0 "First Lease!"
    it "throws a 404 for missing items" $ \env ->
      try env (getLease 42) `shouldThrow` (\e -> responseStatus e == notFound404)

withClient :: IO Application -> SpecWith ClientEnv -> SpecWith ()
withClient x innerSpec =
  beforeAll (newManager defaultManagerSettings) $
  flip aroundWith innerSpec $ \action manager ->
    testWithApplication x $ \port ->
      let baseUrl = BaseUrl Http "localhost" port ""
      in action (ClientEnv manager baseUrl)

try :: ClientEnv -> ClientM a -> IO a
try clientEnv action = either throwIO return =<< runClientM action clientEnv
