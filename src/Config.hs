module Config where

import           Data.ByteString.Char8 as B
import           Data.Text             as T
import           System.Environment
import           Web.Stripe


type TrelloKey = T.Text
type TrelloToken = T.Text
type TrelloList = T.Text


data Config = Config
    { stripeConfig :: StripeConfig
    , trelloKey    :: TrelloKey
    , trelloToken  :: TrelloToken
    , listId       :: TrelloList
    , port         :: Int
    , environment  :: String
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
