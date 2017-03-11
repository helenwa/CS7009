{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
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
        :<|> "fdg" :> Capture "x" String :> Capture "y" Int :> Get '[JSON] Position

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
        fdg :: String -> Int -> Handler Position
        fdg userName ttl = return (crawl userName ttl)

users :: [UserL]
users = [ UserL 1 "Isaac" "Newton"
        , UserL 2 "Albert" "Einstein"
        ]
crawl :: String -> Int -> Position
crawl name ttl = do 
    let repositorys = repos $ pack name 
        repNo = Data.List.length repositorys
        p = (Position name repNo)
    return liftIO p
   
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
    

