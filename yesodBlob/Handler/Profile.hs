module Handler.Profile where

import Import
import qualified GitHub

getProfileR :: Handler Html 
getProfileR = do
    (_, user) <- requireAuthPair
    
    sess <- getSession
    let log = lookup "login" sess
    let token = lookup "access_token" sess
    defaultLayout $ do
        setTitle . toHtml $ userIdent user  <> "'s User page"
        $(widgetFile "profile")

    
    
    --possibleUser <- GitHub.executeRequest' $ GitHub.userInfoCurrentR
    --let possibleUser = GitHub.userInfoCurrentR
    --let possibleUser = GitHub.userInfoCurrentR
    --case possibleUser of
    ----   (Left error)  -> putStrLn $ "Error: " ++ (show error)
    --   (Right user) -> putStrLn $ intercalate "\n\n" $ user