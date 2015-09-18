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
import Web.Stripe.Customer
import Network.Wai
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import Data.List (foldl')
import Data.ByteString.Char8 (pack)

data StripeMode
    = Live
    | Test


type ChargeAPI = "charge" :> ReqBody '[FormUrlEncoded] TokenInfo :> Post '[JSON] TokenInfo


data TokenInfo = TokenInfo
 { stripeToken :: T.Text
 , stripeEmail :: T.Text
 , stripeTokenType :: Maybe T.Text
 , stripeBillingName :: Maybe T.Text
 , stripeBillingAddressLine1 :: Maybe T.Text
 , stripeBillingAddressZip :: Maybe T.Text
 , stripeBillingAddressState :: Maybe T.Text
 , stripeBillingAddressCity :: Maybe T.Text
 , stripeBillingAddressCountry :: Maybe T.Text
 , stripeShippingName :: Maybe T.Text
 , stripeShippingAddressLine1 :: Maybe T.Text
 , stripeShippingAddressZip :: Maybe T.Text
 , stripeShippingAddressState :: Maybe T.Text
 , stripeShippingAddressCity :: Maybe T.Text
 , stripeShippingAddressCountry :: Maybe T.Text
 } deriving (Show, Generic)


getKey :: StripeMode -> IO String
getKey Live = getEnv "STRIPE_LIVE_SECRET_KEY"
getKey Test = getEnv "STRIPE_TEST_SECRET_KEY"


instance FromFormUrlEncoded TokenInfo where
    fromFormUrlEncoded :: [(T.Text,T.Text)] -> Either String TokenInfo
    fromFormUrlEncoded requestParams = do
        let requestParamsMap = M.fromList requestParams
        if
            M.member "stripeToken" requestParamsMap &&
            M.member "stripeEmail" requestParamsMap
        then
            return TokenInfo
            { stripeToken = M.lookupDefault "" "stripeToken" requestParamsMap
            , stripeEmail = M.lookupDefault "" "stripeEmail" requestParamsMap
            , stripeTokenType = M.lookup "stripeTokenType" requestParamsMap
            , stripeBillingName = M.lookup "stripeBillingName" requestParamsMap
            , stripeBillingAddressLine1 = M.lookup "stripeBillingAddressLine1" requestParamsMap
            , stripeBillingAddressZip = M.lookup "stripeBillingAddressZip" requestParamsMap
            , stripeBillingAddressState = M.lookup "stripeBillingAddressState" requestParamsMap
            , stripeBillingAddressCity = M.lookup "stripeBillingAddressCity" requestParamsMap
            , stripeBillingAddressCountry = M.lookup "stripeBillingAddressCountry" requestParamsMap
            , stripeShippingName = M.lookup "stripeShippingName" requestParamsMap
            , stripeShippingAddressLine1 = M.lookup "stripeShippingAddressLine1" requestParamsMap
            , stripeShippingAddressZip = M.lookup "stripeShippingAddressZip" requestParamsMap
            , stripeShippingAddressState = M.lookup "stripeShippingAddressState" requestParamsMap
            , stripeShippingAddressCity = M.lookup "stripeShippingAddressCity" requestParamsMap
            , stripeShippingAddressCountry = M.lookup "stripeShippingAddressCountry" requestParamsMap
            }
        else
            Left "Didn't contain stripeToken and stripeEmail"



instance ToJSON TokenInfo



server :: Server ChargeAPI
server = charge

-- createCustomer :: StripeConfig -> TokenId


charge :: TokenInfo -> EitherT ServantErr IO TokenInfo
charge tokenInfo = do
       keyString <- liftIO $ getKey Test
       let config = StripeConfig (pack keyString)
       let tokenID = TokenId (stripeToken tokenInfo)
       customerResult <- (liftIO $ stripe config (createCustomerByToken tokenID) ) :: EitherT ServantErr IO (Either StripeError Customer)
       case customerResult of
           Left stripeErr -> left err500 -- TODO Customize Error
           Right customer -> right customer
       -- TODO Create and submit charge
       -- TODO Refactor customer creation and charge creation, write unit tests, handle non-new customer case
       return tokenInfo


chargeAPI :: Proxy ChargeAPI
chargeAPI = Proxy


app :: Application
app = serve chargeAPI server
