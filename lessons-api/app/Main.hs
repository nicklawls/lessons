module Main where

import Lessons
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger


-- pull out Live/Test split into main, pick a logger


main :: IO ()
main =
    do print "Lessons running on port 8081"
       run 8081 $ logStdoutDev (checkoutCors app)


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
