{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Lessons (app) where

import System.Environment
import Control.Monad.Trans.Either
import Control.Monad.Trans
import Servant
import Data.Aeson
import GHC.Generics
import Web.Stripe
import Web.Stripe.Token
import Web.Stripe.Charge
import Web.Trello
import Network.Wai
import qualified Data.Text as T
import Data.ByteString.Char8 (pack)
import Data.Monoid

-- TODO Create Config.hs with all the api keys and deployment state, and wrap the server api in ReaderT Config, keys should all be text

data StripeMode
    = Live
    | Test


newtype ChargeSuccess = ChargeSuccess
    { chargeID :: T.Text
    } deriving (Generic)


instance ToJSON ChargeSuccess


type ChargeAPI = "charge" :> ReqBody '[JSON] ChargeRequest :> Post '[JSON] ChargeSuccess


data ChargeRequest
 = ChargeRequest
 { stripeToken :: T.Text
 , amount :: Amount -- Amount is exported by Web.Stripe.Charge
 , name :: Name
 , email :: Lessons.Email
 } deriving (Show, Generic)

type Name = T.Text
type Email = T.Text

instance FromJSON ChargeRequest


getStripeKey :: StripeMode -> IO String
getStripeKey Live = getEnv "STRIPE_LIVE_SECRET_KEY"
getStripeKey Test = getEnv "STRIPE_TEST_SECRET_KEY"


getTrelloKey :: IO String
getTrelloKey = getEnv "TRELLO_DEVELOPER_PUBLIC_KEY"


getTrelloToken :: IO String
getTrelloToken = getEnv "TRELLO_MEMBER_TOKEN"

getListId :: IO String
getListId = getEnv "LESSONS_LIST_ID"

-- TODO Unit Tests
executeCharge :: StripeConfig -> TokenId -> Amount -> EitherT ServantErr IO Charge
executeCharge config tokenID amnt =
    do chargeResult <- liftIO . stripe config $ (chargeCardByToken tokenID USD amnt Nothing)
       case chargeResult of
           Left err -> (liftIO . print $ err) >>= const (left err500) -- TODO Customize Error
           Right charge ->
                right charge


charge :: ChargeRequest -> EitherT ServantErr IO ChargeSuccess
charge request =
    do stripeKey <- liftIO $ getStripeKey Test
       let config = StripeConfig (pack stripeKey)
           tokenID = TokenId (stripeToken request)
           amnt = amount request
       chrg <- executeCharge config tokenID amnt
       trelloKey <- liftIO getTrelloKey
       trelloToken <- liftIO getTrelloToken
       listId <- liftIO getListId
       let chargeIdRaw = extractId (chargeId chrg)
       let newCard = cardTemplate amnt (Lessons.name request) (Lessons.email request) chargeIdRaw (T.pack listId)
       trelloResult <- liftIO . runEitherT $ createCard newCard (Just $ T.pack trelloKey) (Just $ T.pack trelloToken)
       case trelloResult of
           Left err ->
                (liftIO . print $ err) >>= const (left err500)
           Right _ ->
            right (ChargeSuccess chargeIdRaw)


-- TODO calculate number of lessons
cardTemplate :: Amount -> Name -> Lessons.Email -> T.Text -> T.Text -> NewCard
cardTemplate a n e c l =
    NewCard ("Purchased Lesson(s) with " <> n) (Just (e <> "\n" <> T.pack (show a) <> "\n" <> "Charge ID: " <> c))  (Just Top) Nothing l Nothing Nothing Nothing Nothing Nothing Nothing



extractId :: ChargeId -> T.Text
extractId (ChargeId text) = text
extractId _ = "Expansion Object"


server :: Server ChargeAPI
server = charge


chargeAPI :: Proxy ChargeAPI
chargeAPI = Proxy


app :: Application
app = serve chargeAPI server
