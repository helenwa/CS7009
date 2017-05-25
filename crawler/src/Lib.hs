{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ExtendedDefaultRules   #-}

module Lib
    ( startApp
    , app
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Data.Text hiding(intercalate, map, lookup)
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)

import DBHelper
import GithubHelper
import N4JHelper
import Servant

type ApiHandler = ExceptT ServantErr IO

type API = "fullList" :> Get '[JSON] ListOb
        :<|> "token" :> Capture "tkn" String :> Get '[JSON] Log
        :<|> "startC" :> Capture "usern" String :> Capture "hops" Integer :> Get '[JSON] Log
        :<|> "fdg" :> Get '[JSON] FDG
        :<|> "userLanguages" :> Capture "id" String :> Get '[JSON] LangList

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = simpleCors ( serve api server)

api :: Proxy API
api = Proxy

server :: Server API
server = fullList
    :<|> token
    :<|> startC
    :<|> fdg
    :<|> userLanguages

fdg :: ApiHandler FDG
fdg = liftIO $ do
    users <- allUsers
    repos <- allRepo
    links <- allLinks
    return $fullFDG users repos links
    
fullList :: ApiHandler ListOb
fullList = liftIO $ do
    users <- allUsers
    repos <- allRepo
    links <- allLinks
    return $ListOb users repos links
    
token :: String -> ApiHandler Log
token tkn =  liftIO $ do
    putStrLn "Output" 
    b <- addAuth tkn
    return (Log "done" 0)

startC :: String -> Integer-> ApiHandler Log
startC usern hops=  liftIO $ do 
    putStrLn "Output"
    let startingUser = UserDB usern
    b <- addUser startingUser
    authDb <- getAuth
    aAuth <- tokenToAuth authDb
    r <- crawlUser hops aAuth startingUser 
    let repNo = 8
    let p = (Log usern repNo)
    return p

    
userLanguages :: String -> ApiHandler LangList
userLanguages useId = liftIO $ do
    all <- allLanguages
    users <- userLanguageList useId
    let lang = LangList all [] users
    return lang
