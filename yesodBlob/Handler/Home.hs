module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Yesod.Auth
import Yesod.Auth.OAuth2.Github
--ADDED


-- Replace with Google client ID.
--clientId :: Text
--clientId = "9a0d46d298b4a566b942"

-- Replace with Google secret ID.
--clientSecret :: Text
--clientSecret = "b4c128887e732dee6eb4ebeaae99e840d86e5d1f"

data App = App
    { httpManager :: Manager
    }
--END ADDED


-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }
-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getAuthR :: Handler Html

getHomeR :: Handler Html
getHomeR = do
    sess <- getSession
    
    maid <- maybeAuthId
    --defaultLayout
    --    [whamlet|
    --        <p>Your current auth ID: #{show maid}
    --        $maybe _ <- maid
    --            <p>
    --                <a href=@{AuthR LogoutR}>Logout
    --        $nothing
    --            <p>
    --                <a href=@{AuthR LoginR}>Go to the login page
    --    |]
--END ADDED
postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField textSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "What's on the file?"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "File description")
                ]
            }

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
