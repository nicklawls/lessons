module Main where

import Lessons
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Config


main :: IO ()
main =
    do config <- getConfig
       let logger = case environment config of
                        "TEST" -> logStdoutDev
                        "PROD" -> logStdout
                        _      -> error "LESSONS_ENV must be TEST or PROD"
       print $ "Lessons Running on port " ++ show (port config)
       run (port config) $ logger (checkoutCors $ app config)




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
