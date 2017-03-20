{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}

module GithubHelper where

import GitHub
import GitHub.Data.Repos
import GitHub.Endpoints.Repos

import Data.Text hiding(intercalate, map, lookup)
import Data.Vector hiding(map, mapM)
import Data.Aeson
import GHC.Generics
import Data.Maybe
import DBHelper

data RepoInfo = RepoInfo{
    name::Text,
    size::Integer
}deriving(ToJSON, FromJSON, Generic, Eq, Show)

--Get users repositorys   
repos :: Text -> IO[RepoInfo]
repos userName = do
  possibleRepos <- GitHub.Endpoints.Repos.userRepos (mkOwnerName userName) GitHub.Data.Repos.RepoPublicityAll
  case possibleRepos of
       (Left error)  -> do 
            return $ ([])
            
       (Right repos) -> do 
            x <- mapM formatRepoDB repos
            return $ Data.Vector.toList x 
         
formatRepo :: GitHub.Repo -> IO(RepoInfo)
formatRepo repo = do
    let name = untagName (GitHub.Data.Repos.repoName repo)
    size <-  formatNumber (GitHub.Data.Repos.repoSize repo)
    return (RepoInfo name size)
    
formatRepoDB :: String -> GitHub.Repo -> IO(RepoDB)
formatRepo userName repo = do
    let name = untagName (GitHub.Data.Repos.repoName repo)
    let id = userName + name
    --size <-  formatNumber (GitHub.Data.Repos.repoSize repo)
    return (RepoDB id name)
            
formatNumber :: Maybe Int -> IO(Integer)
formatNumber n =
    let r = toEnum $ fromMaybe 0 n
    in return r
    
crawlUser :: userDB -> Int -> IO
crawlUser user ttl = do 
    let repositorys = repos $ userId user
    x <- mapM addRepo repositorys
    --do links
    if (ttl>0)
        mapM crawlRepo repositorys $ ttl - 1
            
    
crawlRepo :: repoDB -> Int -> IO
crawlRepo repo ttl = do 
    let users = usersOf $ repoId repo
    