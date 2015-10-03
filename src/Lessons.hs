{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Lessons (app) where

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
import Data.Monoid
import Control.Monad.Reader
import Config

-- TODO Put the whole thing in stm and do the trello call async


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


-- TODO Unit Tests
executeCharge :: StripeConfig -> TokenId -> Amount -> LessonsM Charge
executeCharge config tokenID amnt =
    do chargeResult <- liftIO . stripe config $ (chargeCardByToken tokenID USD amnt Nothing)
       case chargeResult of
          Left err ->
            printLeft err
          Right charge ->
            return charge


executeTrello :: NewCard -> T.Text -> T.Text -> LessonsM Card
executeTrello newCard key token =
    do result <- liftIO . runEitherT $ createCard newCard (Just key) (Just token)
       case result of
           Left err ->
               printLeft err
           Right card ->
               return card


-- print the error to the console before calling left on err500
-- TODO Customize Error
printLeft :: Show a => a -> LessonsM b
printLeft err =
    do liftIO (print err)
       lift (left err500)


chargeLessons :: ChargeRequest -> ReaderT Config (EitherT ServantErr IO) ChargeSuccess
chargeLessons request =
    do config <- ask
       let tokenID = TokenId (stripeToken request)
           amnt = amount request
       chrg <- executeCharge (stripeConfig config) tokenID amnt
       let chargeIdRaw = extractId (chargeId chrg)
           newCard =
               cardTemplate amnt (Lessons.name request) (Lessons.email request) chargeIdRaw (listId config)
       _ <- executeTrello newCard (trelloKey config) (trelloToken config)
       return ChargeSuccess {chargeID = chargeIdRaw}
       -- `right` only works when EitherT is the top of the stack


-- TODO calculate number of lessons
-- TODO Separate Card Creation from Domain-Specific information to be included in the card
cardTemplate :: Amount -> Name -> Lessons.Email -> T.Text -> T.Text -> NewCard
cardTemplate a n e c l =
    NewCard ("Purchased Lesson(s) with " <> n) (Just message) (Just Top) Nothing l Nothing Nothing Nothing Nothing Nothing Nothing
        where
            message = e <> "\n" <> T.pack (show a) <> "\n" <> "Charge ID: " <> c


extractId :: ChargeId -> T.Text
extractId (ChargeId text) = text
extractId _ = "Expansion Object"


type LessonsM = ReaderT Config (EitherT ServantErr IO)


chargeAPI :: Proxy ChargeAPI
chargeAPI = Proxy


readerToEither :: Config -> LessonsM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg


readerServer :: Config -> Server ChargeAPI
readerServer cfg = enter (readerToEither cfg) server


server :: ServerT ChargeAPI LessonsM
server = chargeLessons


app :: Config -> Application
app cfg = serve chargeAPI (readerServer cfg)
