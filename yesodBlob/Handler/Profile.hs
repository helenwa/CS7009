module Handler.Profile where

import Import
import qualified GitHub

getProfileR :: Handler Html 
getProfileR = do
    (_, user) <- requireAuthPair
    
    sess <- getSession
    defaultLayout $ do
        setTitle . toHtml $ userIdent user  <> "'s User page"
        $(widgetFile "profile")

    
    
    --possibleUser <- GitHub.executeRequest' $ GitHub.userInfoCurrentR
    --let possibleUser = GitHub.userInfoCurrentR
    --let possibleUser = GitHub.userInfoCurrentR
    --case possibleUser of
    ----   (Left error)  -> putStrLn $ "Error: " ++ (show error)
    --   (Right user) -> putStrLn $ intercalate "\n\n" $ user