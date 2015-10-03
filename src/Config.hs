module Config where

import Web.Stripe
import Data.Text as T
import System.Environment
import Data.ByteString.Char8 as B
import Control.Monad.Trans

type TrelloKey = T.Text
type TrelloToken = T.Text
type TrelloList = T.Text

data Config = Config
    { stripeConfig :: StripeConfig
    , trelloKey :: TrelloKey
    , trelloToken :: TrelloToken
    , listId :: TrelloList
    , port :: Int
    , environment :: String
    }

getConfig :: IO Config
getConfig =
    Config
       <$> fmap StripeConfig (packEnv "STRIPE_SECRET_KEY")
       <*> packEnvText "TRELLO_DEVELOPER_PUBLIC_KEY"
       <*> packEnvText "TRELLO_MEMBER_TOKEN"
       <*> packEnvText "LESSONS_LIST_ID"
       <*> fmap read (getEnv "PORT")
       <*> getEnv "LESSONS_ENV"

packEnv :: String -> IO B.ByteString
packEnv = fmap B.pack . getEnv

packEnvText :: String -> IO T.Text
packEnvText = fmap T.pack . getEnv


getTrelloKey :: IO String
getTrelloKey = getEnv "TRELLO_DEVELOPER_PUBLIC_KEY"


getTrelloToken :: IO String
getTrelloToken = getEnv "TRELLO_MEMBER_TOKEN"

getListId :: IO String
getListId = getEnv "LESSONS_LIST_ID"
