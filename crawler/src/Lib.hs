{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Data.List hiding(intercalate, map, lookup)
import Data.Text.Encoding
import Data.Vector hiding(map, mapM)
import Data.Text hiding(intercalate, map, lookup)
import Data.Maybe

import GitHub
import GitHub.Data.Repos
import GitHub.Endpoints.Repos
import Control.Monad.IO.Class (liftIO)

import DBHelper

import Control.Monad.Trans.Except

type ApiHandler = ExceptT ServantErr IO

data UserL = UserL
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)
$(deriveJSON defaultOptions ''UserL)
  
data Position = Position
  { xCoord :: String
  , yCoord :: Int
  } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Position)



data RepoInfo = RepoInfo{
    name::Text,
    size::Integer
}deriving(Eq, Show)
$(deriveJSON defaultOptions ''RepoInfo)

type API = "userList" :> Get '[JSON] [UserL]
        :<|> "user" :> Capture "x" String :> Capture "y" Int :> Get '[JSON] Position
        :<|> "startC" :> ReqBody '[JSON] UserL :> Post '[JSON] Position

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
        user userName ttl = return (Position userName ttl)

startC :: UserL -> ApiHandler Position
startC (UserL userId userN text) =  liftIO $ do 
    putStrLn "Output"
    let repositorys = repos $ pack userN 
    let repNo = 8--Data.List.length repositorys
    let p = (Position userN repNo)
    return p

users :: [UserL]
users = [ UserL 1 "Isaac" "Newton"
        , UserL 2 "Albert" "Einstein"
        ]
-- crawl :: String -> Int -> Position
-- crawl name ttl = do 
    -- let repositorys = repos $ pack name 
        -- repNo = Data.List.length repositorys
        -- p = (Position name repNo)
    -- return  p
   
--Get users repositorys   
repos :: Text -> IO[RepoInfo]
repos userName = do
  possibleRepos <- GitHub.Endpoints.Repos.userRepos (mkOwnerName userName) GitHub.Data.Repos.RepoPublicityAll
  case possibleRepos of
       (Left error)  -> do 
            return $ ([RepoInfo (pack "Error")0])
            
       (Right repos) -> do 
            x <- mapM formatRepo repos
            return $ Data.Vector.toList x 
         
formatRepo :: GitHub.Repo -> IO(RepoInfo)
formatRepo repo = do
	let name = untagName (GitHub.Data.Repos.repoName repo)
	size <-  formatNumber (GitHub.Data.Repos.repoSize repo)
	return (RepoInfo name size)
            
formatNumber :: Maybe Int -> IO(Integer)
formatNumber n =
    let r = toEnum $ fromMaybe 0 n
    in return r
    

