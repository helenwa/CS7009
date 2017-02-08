module Handler.Profile where

import Import hiding (unpack, pack, Auth)
import Data.List hiding(intercalate, map, lookup)
import GitHub
import GitHub.Data.Repos
import GitHub.Endpoints.Repos
--import GitHub.Data.Activities
import GitHub.Endpoints.Activity.Starring
import Data.Maybe
import Data.Text.Encoding
import Data.Text hiding(intercalate, map, lookup)
import Data.Map hiding(intercalate, map, lookup)

getProfileR :: Handler Html 
getProfileR = do

    (_, user) <- requireAuthPair
    sess <- getSession
    let log = lookup "login" sess
    let token = lookup "access_token" sess
    let textName = Data.Text.Encoding.decodeUtf8 (fromJust log)
    repositorys <- liftIO $ repos textName
    stared <- liftIO $ stars textName Nothing 
    let repNo = Data.List.length repositorys
    defaultLayout $ do
        setTitle . toHtml $ userIdent user  <> "'s User page"
        $(widgetFile "profile")

--Get users repositorys   
repos :: Text -> IO (String)
repos userName = do
  possibleRepos <- GitHub.Endpoints.Repos.userRepos (mkOwnerName userName) GitHub.Data.Repos.RepoPublicityAll
  case possibleRepos of
       (Left error)  -> do 
            return $ "Error: " Data.List.++ (show error)
            
       (Right repos) -> do 
            return $ intercalate "\n\n" $ map show repos
            
--Get repositorys stared by user
stars :: Text -> Maybe Auth-> IO (String)
stars userName auth = do
  starRepos <- GitHub.Endpoints.Activity.Starring.reposStarredBy auth (mkOwnerName userName) --GitHub.Data.Repos.RepoPublicityAll
  case starRepos of
       (Left error)  -> do 
            return $ "Error: " Data.List.++ (show error)
       (Right repos) -> do 
            return $ intercalate "\n\n" $ map show repos
 