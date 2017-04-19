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
import GitHub.Auth

import Control.Monad.IO.Class (liftIO)

import Data.Text hiding(intercalate, map, lookup)
import Data.Vector hiding(map, mapM, mapM_)
import Data.Aeson
import GHC.Generics
import Data.Maybe
import DBHelper
import Data.Text.Internal
import qualified Data.ByteString.Char8 as BS

--Get users repositorys   
reposOf :: Text -> GitHub.Auth.Auth -> IO[RepoDB]
reposOf userName auth= do
  possibleRepos <- GitHub.Endpoints.Repos.userRepos' (Just auth) (mkOwnerName userName) GitHub.Data.Repos.RepoPublicityAll
  case possibleRepos of
       (Left error)  -> do 
            return $ ([])
       (Right repos) -> do 
            x <- mapM formatRepoDB repos
            return $ Data.Vector.toList x 
            
usersOf :: RepoDB -> GitHub.Auth.Auth -> IO[UserDB]
usersOf repo auth = do
    possibleUsers <- GitHub.Endpoints.Repos.Collaborators.collaboratorsOn' (Just auth)  (mkOwnerName (pack(DBHelper.repoOwner repo))) (mkRepoName (pack(DBHelper.repoName repo)))
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
    putStrLn $ unpack $ name
    return (UserDB (unpack name) )
    
crawlUser :: Integer -> GitHub.Auth.Auth -> UserDB ->  IO()
crawlUser ttl auth user  = do 
    putStrLn "user"
    putStrLn $ show ttl
    repositorys <- reposOf (pack ( DBHelper.userId user)) auth
    x <- mapM addRepo repositorys
    links <- mapM (makeLink userRepo owns user) repositorys
    y <- mapM addLink links
    case (ttl) of 
             0 ->  putStrLn "ended on "
             _ ->  mapM_ (crawlRepo (ttl - 1) auth) repositorys 
    
crawlRepo :: Integer -> GitHub.Auth.Auth -> RepoDB  -> IO()
crawlRepo ttl auth repo  = do 
    putStrLn "repo"
    putStrLn $ show ttl
    ppl <- usersOf repo auth
    x <- mapM addUser ppl
    links <- mapM (makeLink2 userRepo contributesTo repo) ppl
    y <- mapM addLink links
    if (ttl>0)
        then mapM_ (crawlUser (ttl - 1) auth) ppl
        else putStrLn "ended on "
        
tokenToAuth :: Text -> IO(GitHub.Auth.Auth)
tokenToAuth tkn = do
    let t = GitHub.Auth.OAuth $ BS.pack $ unpack tkn
    return t