module Main where

import Lib
import Network.Wai.Handler.Warp



main :: IO ()
main = do
    stripeSecretKey <- getKey Test
    print stripeSecretKey
    run 8081 app
