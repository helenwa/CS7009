{-# LANGUAGE PackageImports #-}
{-
    You need to register your Twitter application at <http://dev.twitter.com/>
    to get the consumer key and secret needed for OAuth.  When connecting to
    Twitter for the first time, do this:
        let consumer = Consumer "consumer key" "consumer secret"
        token <- authenticate
    This authenticates your application against Twitter. You should save the
    token. It has a Binary instance, so you could just use encodeFile from
    Data.Binary.
    If you are the only user of your Twitter application, you can choose to use
    the single access token. In that case use singleAccessToken to generate a
    token - you can run it every time as it's actually a pure operation
    (although its type doesn't look like that).
        token <- singleAccessToken consumer "access token" "access secret"
    Now you are ready to tweet:
        tweet consumer token "Hello world!"
        
-}
module Web.Twitter.Simple where

import Data.Maybe (fromJust)
import "mtl" Control.Monad.Trans
import Network.OAuth.Consumer
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import Network.OAuth.Http.HttpClient
import Network.OAuth.Http.PercentEncoding

reqUrl = fromJust . parseURL $ "https://api.twitter.com/oauth/request_token"
accUrl = fromJust . parseURL $ "https://api.twitter.com/oauth/access_token"

tweetUrl = fromJust . parseURL $ "http://api.twitter.com/1/statuses/update.json"

authUrl = ("https://api.twitter.com/oauth/authorize?oauth_token=" ++)
            . findWithDefault ("oauth_token","") . oauthParams

data Consumer = Consumer
    { key :: String
    , secret :: String }
    deriving (Show, Eq)

authenticate :: Consumer -> IO Token
authenticate consumer = unCurlM . runOAuth $ do
    ignite $ Application (key consumer) (secret consumer) OOB 
    oauthRequest HMACSHA1 Nothing reqUrl
    cliAskAuthorization authUrl
    oauthRequest HMACSHA1 Nothing accUrl
    getToken

singleAccessToken :: Consumer -> String -> String -> IO Token
singleAccessToken consumer accToken accSecret = unCurlM . runOAuth $ do
    let app = Application (key consumer) (secret consumer) OOB
    let newToken = [("oauth_token", accToken)
                   ,("oauth_token_secret", accSecret)]
    ignite app
    token <- getToken
    return $ AccessToken app (fromList newToken `union` oauthParams token)

tweet :: Consumer -> Token -> String -> IO Response
tweet consumer token message = unCurlM . runOAuth $ do
    ignite $ Application (key consumer) (secret consumer) OOB
    putToken token
    -- I can't figure out how to put the status in the POST body, but putting
    -- it in the query string works.
    let body = "status=" ++ encode message
        request = tweetUrl { method = POST
                           , qString = fromList [("status", message)]
                           }
    serviceRequest HMACSHA1 Nothing request