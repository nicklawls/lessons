{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}

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
import Network.Wai
import qualified Data.Text as T
import Data.HashMap.Strict as D

data StripeMode
    = Live
    | Test

getKey :: StripeMode -> IO String
getKey Live = getEnv "STRIPE_LIVE_SECRET_KEY"
getKey Test = getEnv "STRIPE_TEST_SECRET_KEY"

type ChargeAPI = "charges" :> ReqBody '[FormUrlEncoded] TokenInfo :> Post '[JSON] TokenInfo

data TokenInfo = TokenInfo
 { stripeToken :: T.Text
 , stripeTokenType :: T.Text
 , stripeEmail :: T.Text
 } deriving (Show, Generic)

instance FromFormUrlEncoded TokenInfo where
    fromFormUrlEncoded :: [(T.Text,T.Text)] -> Either String TokenInfo
    fromFormUrlEncoded params =
        if length params == 3
            then
                let (_,vals) = unzip params
                in  Right (TokenInfo  (vals !! 0) (vals !! 1) (vals !! 2))
            else Left "doesn't have the magic 3"



instance ToJSON TokenInfo

server :: Server ChargeAPI
server = charges
    where
        charges :: TokenInfo -> EitherT ServantErr IO TokenInfo
        charges tokenInfo = do
            liftIO . print $ tokenInfo
            return tokenInfo


chargeAPI :: Proxy ChargeAPI
chargeAPI = Proxy

app :: Application
app = serve chargeAPI server
