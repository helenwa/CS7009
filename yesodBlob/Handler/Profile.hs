module Handler.Profile where

import Import
import qualified GitHub

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    --(_, possibleUser) <- GitHub.User
    defaultLayout $ do
        setTitle . toHtml $ userIdent user  <> "'s User page"
        $(widgetFile "profile")
