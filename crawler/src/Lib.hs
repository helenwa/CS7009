{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)
  
data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Position)

$(deriveJSON defaultOptions ''User)

type API = "userList" :> Get '[JSON] [User]
        :<|> "fdg" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = userList
    :<|> fdg
    
    where 
        userList = return users
        fdg :: Int -> Int -> Handler Position
        fdg x y = return (Position x y)

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

