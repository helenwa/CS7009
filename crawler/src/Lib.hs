{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

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
import Servant

type ApiHandler = ExceptT ServantErr IO

type API = "userList" :> Get '[JSON] [Log]
        :<|> "token" :> Capture "tkn" String :> Get '[JSON] Log
        :<|> "user" :> Capture "id" String :> Capture "hops" Int :> Get '[JSON] Log
        :<|> "startC" :> Capture "usern" String :> Capture "hops" Integer :> Get '[JSON] Log

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = simpleCors ( serve api server)

api :: Proxy API
api = Proxy

server :: Server API
server = userList
    :<|> token
    :<|> user
    :<|> startC
    where
    userList = return users

users :: [Log]
users = [ Log "One" 1
        , Log "Two" 2
        ]

user :: String -> Int -> ApiHandler Log
user userName ttl =  liftIO $ do
    putStrLn "Output"
    let newU = UserDB userName  
    b <- addUser newU
    return (Log userName ttl)
    
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


