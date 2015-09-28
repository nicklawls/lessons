{-# LANGUAGE DeriveGeneric #-}

module Poop.Trello.Card where

import qualified Data.Text as T
import Data.Aeson
import GHC.Generics
import Poop.Trello.Badges


data Card = Card
    { badges :: Badges
    , checkItemStates :: [T.Text]
    , closed :: Bool
    , dateLastActivity :: T.Text
    , desc :: T.Text
    , descData :: DescData
    , due :: Maybe T.Text
    , email :: T.Text
    , id :: T.Text
    , idAttachmentCover :: Maybe T.Text
    , idBoard :: T.Text
    , idChecklists :: [T.Text]
    , idLabels :: [T.Text]
    , idList :: T.Text
    , idMembers :: [T.Text]
    , idShort :: Int
    , labels :: [T.Text]
    , manualCoverAttachment :: Bool
    , name :: T.Text
    , pos :: Int -- TODO encode in position type
    , shortUrl :: T.Text
    , stickers :: [T.Text]
    , url :: T.Text
    } deriving (Show, Generic)

instance FromJSON Card
