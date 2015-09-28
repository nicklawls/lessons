{-# LANGUAGE DeriveGeneric #-}

module Web.Trello.Badges where

import qualified Data.Text as T
import Data.Aeson
import GHC.Generics


data Badges = Badges
    { votes :: Int
    , viewingMemberVoted :: Bool
    , subscribed :: Bool
    , fogbugz :: T.Text
    , checkItems :: Int
    , checkItemsChecked :: Int
    , comments :: Int
    , attachments :: Int
    , description :: Bool
    , due :: Maybe T.Text
    } deriving (Show, Generic)

instance FromJSON Badges

data DescData = DescData
    { emoji :: Object -- TODO Figure out what's in emoji
    } deriving (Show, Generic)

instance FromJSON DescData
