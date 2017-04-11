{-# LANGUAGE OverloadedStrings    #-}


module Handler.Req where

import Import hiding (newManager, defaultManagerSettings, httpLbs)
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

callCrawler :: String -> IO ()
callCrawler tkn = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest "http://localhost:8080/user/FromProfile/1"
  response <- httpLbs request manager
  putStrLn $ pack $ show $ responseBody response
