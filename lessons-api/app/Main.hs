module Main where

import Lib
import Network.Wai.Handler.Warp



main :: IO ()
main = do
    print "Lessons running on port 8081 " 
    run 8081 app
