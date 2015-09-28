{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Trello where

import qualified Data.Text as T
import Data.Aeson
import GHC.Generics
import Servant
import Web.Trello.Card
import Control.Monad.Trans.Either
import Servant.Client


type Key = T.Text
type AuthToken = T.Text
type AuthParams a = QueryParam "key" Key :> QueryParam "token" AuthToken :> a
type TrelloAPI = "1" :> "cards" :> ReqBody '[JSON] NewCard :> AuthParams (Post '[JSON] Card)


data Position
    = Top
    | Bottom
    | Int
    deriving (Show)


-- in Google Translate, he implements Show and ToText instead, then toJSON = String . toText
instance ToJSON Position where
    toJSON position =
        case position of
            Top ->
                toJSON ("top" :: T.Text)
            Bottom ->
                toJSON ("bottom" :: T.Text)
            n ->
                toJSON n


data NewCard = NewCard
    { name           :: T.Text -- optional when copying, added anyway
    , desc           :: Maybe T.Text -- length 0-16384
    , pos            :: Maybe Position
    , due            :: Maybe T.Text -- TODO replace with an api-compliant date type
    , idList         :: T.Text -- id of list being added to
    , idMembers      :: Maybe T.Text
    , idLabels       :: Maybe T.Text
    , urlSource      :: Maybe T.Text -- http://, https:// or null
    , fileSource     :: Maybe T.Text
    , idCardSource   :: Maybe T.Text
    , keepFromSource :: Maybe T.Text
    } deriving (Show, Generic)


instance ToJSON NewCard


trelloAPI :: Proxy TrelloAPI
trelloAPI = Proxy


createCard :: NewCard -> Maybe Key -> Maybe AuthToken -> EitherT ServantError IO Card
createCard = client trelloAPI (BaseUrl Https "api.trello.com" 443)
