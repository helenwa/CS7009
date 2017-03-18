{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    , app
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Data.Text hiding(intercalate, map, lookup)
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)

import DBHelper
import GithubHelper
import Servant

type ApiHandler = ExceptT ServantErr IO

type API = "userList" :> Get '[JSON] [Log]
        :<|> "user" :> Capture "id" String :> Capture "hops" Int :> Get '[JSON] Log
        :<|> "startC" :> ReqBody '[JSON] UserDB :> Post '[JSON] Log

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = userList
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
    let newU = UserDB ttl userName 
    b <- addUser newU
    return (Log userName ttl)

startC :: UserDB -> ApiHandler Log
startC (UserDB h userN) =  liftIO $ do 
    putStrLn "Output"
    let repositorys = repos $ pack userN 
    let repNo = 8
    let p = (Log userN repNo)
    return p


