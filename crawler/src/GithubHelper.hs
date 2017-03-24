{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ExtendedDefaultRules   #-}

module GithubHelper where

import GitHub
import GitHub.Data.Repos
import GitHub.Endpoints.Repos
import GitHub.Endpoints.Repos.Collaborators
import GitHub.Data.Definitions

import Control.Monad.IO.Class (liftIO)

import Data.Text hiding(intercalate, map, lookup)
import Data.Vector hiding(map, mapM, mapM_)
import Data.Aeson
import GHC.Generics
import Data.Maybe
import DBHelper

--Get users repositorys   
reposOf :: Text -> IO[RepoDB]
reposOf userName = do
  possibleRepos <- GitHub.Endpoints.Repos.userRepos (mkOwnerName userName) GitHub.Data.Repos.RepoPublicityAll
  case possibleRepos of
       (Left error)  -> do 
            return $ ([])
       (Right repos) -> do 
            x <- mapM formatRepoDB repos
            return $ Data.Vector.toList x 
            
usersOf :: RepoDB -> IO[UserDB]
usersOf repo = do
    possibleUsers <- GitHub.Endpoints.Repos.Collaborators.collaboratorsOn  (mkOwnerName (pack(DBHelper.repoOwner repo))) (mkRepoName (pack(DBHelper.repoName repo)))
    case possibleUsers of
       (Left error)  -> do 
            return $ ([])
       (Right users) -> do 
            x <- mapM formatUserDB users
            return $ Data.Vector.toList x 
             
formatRepoDB :: GitHub.Repo -> IO(RepoDB)
formatRepoDB repo = do
    let name = untagName (GitHub.Data.Repos.repoName repo)
    let owner = (GitHub.Data.Repos.repoOwner repo)
    let ownerName = untagName $ GitHub.Data.Definitions.simpleOwnerLogin owner
    sizeOf <- liftIO $ formatNumber (GitHub.Data.Repos.repoSize repo)
    return (RepoDB (unpack name) (unpack ownerName) sizeOf)
            
formatNumber :: Maybe Int -> IO(Int)
formatNumber n = do
    return $ fromMaybe (toEnum 0) n
    
formatUserDB :: GitHub.Data.Definitions.SimpleUser -> IO(UserDB)
formatUserDB user = do
    let name = untagName $ GitHub.Data.Definitions.simpleUserLogin user
    return (UserDB (unpack name) )
    
crawlUser :: Int -> UserDB -> IO()
crawlUser ttl user  = do 
    repositorys <- reposOf $ pack $ DBHelper.userId user
    x <- mapM addRepo repositorys
    links <- mapM (makeLink userRepo owns user) repositorys
    if (ttl>0)
        then mapM_ (crawlRepo (ttl - 1)) repositorys
        else putStrLn "ended on "
    
crawlRepo :: Int -> RepoDB ->  IO()
crawlRepo ttl repo  = do 
    ppl <- usersOf repo
    x <- mapM addUser ppl
    links <- mapM (makeLink2 userRepo contributesTo repo) ppl
    if (ttl>0)
        then mapM_ (crawlUser (ttl - 1)) ppl
        else putStrLn "ended on "
    