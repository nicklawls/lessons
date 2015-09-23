{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( getKey
    , StripeMode(Live,Test)
    , app
    ) where

import System.Environment
import Control.Monad.Trans.Either
import Control.Monad.Trans
import Servant
import Data.Aeson
import GHC.Generics
import Web.Stripe
import Web.Stripe.Token
import Web.Stripe.Charge
import Network.Wai

import qualified Data.Text as T
import Data.ByteString.Char8 (pack)

data StripeMode
    = Live
    | Test

newtype ChargeSuccess = ChargeSuccess
    { chargeID :: T.Text
    } deriving (Generic)


instance ToJSON ChargeSuccess
instance FromJSON ChargeRequest


type ChargeAPI = "charge" :> ReqBody '[JSON] ChargeRequest :> Post '[JSON] ChargeSuccess



data ChargeRequest
 = ChargeRequest
 { stripeToken :: T.Text
 , amount :: Amount -- Ammount is exported by Web.Stripe.Charge
 } deriving (Show, Generic)


getKey :: StripeMode -> IO String
getKey Live = getEnv "STRIPE_LIVE_SECRET_KEY"
getKey Test = getEnv "STRIPE_TEST_SECRET_KEY"


-- TODO Unit Tests
executeCharge :: StripeConfig -> TokenId -> Amount -> EitherT ServantErr IO Charge
executeCharge config tokenID amnt = do
    chargeResult <- liftIO $ stripe config (chargeCardByToken tokenID USD amnt Nothing)
    case chargeResult of
        Left stripeErr -> left err500 -- TODO Customize Error
        Right customer -> right customer


charge :: ChargeRequest -> EitherT ServantErr IO ChargeSuccess
charge request = do
       keyString <- liftIO $ getKey Test
       let config = StripeConfig (pack keyString)
       let tokenID = TokenId (stripeToken request)
       let amnt = amount request
       chrg <- executeCharge config tokenID amnt
       right $ ChargeSuccess (extractId . chargeId $ chrg)


extractId :: ChargeId -> T.Text
extractId (ChargeId text) = text
extractId _ = "Expansion Object"


server :: Server ChargeAPI
server = charge


chargeAPI :: Proxy ChargeAPI
chargeAPI = Proxy


app :: Application
app = serve chargeAPI server
