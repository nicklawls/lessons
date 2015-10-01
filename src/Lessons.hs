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
import Web.Trello.Card
import Network.Wai
import qualified Data.Text as T
import Data.ByteString.Char8 (pack)
import Data.Monoid
import Control.Monad.Reader
import Config

-- TODO Create Config.hs with all the api keys and deployment state, and wrap the server api in ReaderT Config, keys should all be text
-- TODO Put the whole bitch in stm and do the trello call async

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
           Left err ->
                printLeft err
           Right charge ->
                right charge


executeTrello :: NewCard -> T.Text -> T.Text -> EitherT ServantErr IO Card
executeTrello newCard key token =
    do result <- liftIO . runEitherT $ createCard newCard (Just key) (Just token)
       case result of
        Left err ->
            printLeft err
        Right card ->
            right card


-- print the error to the console before calling left on err500
-- TODO Customize Error
printLeft :: Show a => a -> EitherT ServantErr IO b
printLeft err = do
    liftIO . print $ err
    left err500



charge :: ChargeRequest -> ReaderT Config (EitherT ServantErr IO) ChargeSuccess
charge request = undefined
    -- do stripeKey <- liftIO $ getStripeKey Test
    --    let config = StripeConfig (pack stripeKey)
    --        tokenID = TokenId (stripeToken request)
    --        amnt = amount request
    --    chrg <- executeCharge config tokenID amnt
    --    trelloKey <- liftIO getTrelloKey
    --    trelloToken <- liftIO getTrelloToken
    --    listId <- liftIO getListId
    --    let chargeIdRaw = extractId (chargeId chrg)
    --        newCard = cardTemplate amnt (Lessons.name request) (Lessons.email request) chargeIdRaw (T.pack listId)
    --    _ <- executeTrello newCard (T.pack trelloKey) (T.pack trelloToken)
    --    right (ChargeSuccess chargeIdRaw)


-- TODO calculate number of lessons
cardTemplate :: Amount -> Name -> Lessons.Email -> T.Text -> T.Text -> NewCard
cardTemplate a n e c l =
    NewCard ("Purchased Lesson(s) with " <> n) (Just message) (Just Top) Nothing l Nothing Nothing Nothing Nothing Nothing Nothing
        where
            message = e <> "\n" <> T.pack (show a) <> "\n" <> "Charge ID: " <> c


extractId :: ChargeId -> T.Text
extractId (ChargeId text) = text
extractId _ = "Expansion Object"

type AppM = ReaderT Config (EitherT ServantErr IO)

chargeAPI :: Proxy ChargeAPI
chargeAPI = Proxy

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

readerServer :: Config -> Server ChargeAPI
readerServer cfg = enter (readerToEither cfg) server

server :: ServerT ChargeAPI AppM
server = charge


app :: Config -> Application
app cfg = serve chargeAPI (readerServer cfg)
