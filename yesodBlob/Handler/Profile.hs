{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
module Handler.Profile where

import Import hiding (unpack, pack, Auth)
import Data.List hiding(intercalate, map, lookup)
import GitHub
import GitHub.Data.Repos
import GitHub.Endpoints.Repos
import GitHub.Endpoints.Activity.Starring
import Data.Maybe
import Data.Text.Encoding
import Data.Vector hiding(map, mapM)
import Data.Text hiding(intercalate, map, lookup)
import GitHub.Auth
import DBHelper

data RepoInfo = RepoInfo{
    name::Text,
    size::Integer
}deriving(ToJSON, FromJSON, Generic, Eq, Show)



getProfileR :: Handler Html 
getProfileR = do

    (_, user) <- requireAuthPair
    sess <- getSession
    let log = lookup "login" sess
    let token = lookup "access_token" sess
    x <- DBHelper.saveToken token
    let textName = Data.Text.Encoding.decodeUtf8 (fromJust log)
    let auth = Just $ GitHub.Auth.OAuth $ fromJust token 
    repositorys <- liftIO $ repos textName
    stared <- liftIO $ stars textName auth  
    let repNo = Data.List.length repositorys
    defaultLayout $ do
        setTitle . toHtml $ userIdent user  <> "'s User page"
        $(widgetFile "profile")

--Get users repositorys   
repos :: Text -> IO[RepoInfo]
repos userName = do
  possibleRepos <- GitHub.Endpoints.Repos.userRepos (mkOwnerName userName) GitHub.Data.Repos.RepoPublicityAll
  case possibleRepos of
       (Left error)  -> do 
            return $ ([RepoInfo (Data.Text.Encoding.decodeUtf8 "Error")0])
            
       (Right repos) -> do 
            x <- mapM formatRepo repos
            return $ Data.Vector.toList x 
         
formatRepo :: GitHub.Repo -> IO(RepoInfo)
formatRepo repo = do
	let name = untagName (GitHub.Data.Repos.repoName repo)
	size <- liftIO $ formatNumber (GitHub.Data.Repos.repoSize repo)
	return (RepoInfo name size)
            
formatNumber :: Maybe Int -> IO(Integer)
formatNumber n =
    let r = toEnum $ fromMaybe 0 n
    
    in return r
    
--Get repositorys stared by user
stars :: Text -> Maybe Auth-> IO (String)
stars userName auth = do
  starRepos <- GitHub.Endpoints.Activity.Starring.reposStarredBy auth (mkOwnerName userName) --GitHub.Data.Repos.RepoPublicityAll
  case starRepos of
       (Left error)  -> do 
            return $ "Error: " Data.List.++ (show error)
       (Right repos) -> do 
            return $ intercalate "\n\n" $ map show repos
 