module Main where

import Lessons
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import System.Environment (getEnv)

-- pull out Live/Test split into main, pick a logger


main :: IO ()
main =
    do portStr <- getEnv "PORT"
       let port = read portStr :: Port
       print ("Lessons running on port " ++ portStr)
       run port $ logStdoutDev (checkoutCors app)


checkoutCors :: Middleware
checkoutCors = cors $ const (Just checkoutCorsResourcePolicy)


checkoutCorsResourcePolicy :: CorsResourcePolicy
checkoutCorsResourcePolicy =
    CorsResourcePolicy
        { corsOrigins = Nothing -- gives you /*
        , corsMethods = simpleMethods
        , corsRequestHeaders = simpleHeaders -- adds "Content-Type" to defaults
        , corsExposedHeaders = Nothing
        , corsMaxAge = Nothing
        , corsVaryOrigin = False
        , corsRequireOrigin = False
        , corsIgnoreFailures = False
        }
