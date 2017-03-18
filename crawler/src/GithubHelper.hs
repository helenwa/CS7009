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
    
-- crawl :: String -> Int -> Position
-- crawl name ttl = do 
    -- let repositorys = repos $ pack name 
        -- repNo = Data.List.length repositorys
        -- p = (Position name repNo)
    -- return  p
    
-- numOf :: IO[RepoInfo] -> Int
-- numOf repos
    -- | repos == IO[] = 0
    -- | otherwise   = Data.List.length repos