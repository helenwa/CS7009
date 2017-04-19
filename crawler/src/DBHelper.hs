{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ExtendedDefaultRules   #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}

module DBHelper where

import Database.Bolt hiding(unpack)
import Data.Text
import Data.Map
import Data.Aeson
import GHC.Generics
import Data.String
import Data.Vector (toList)

data Log = Log
  { word :: String
  , number :: Int
  } deriving (ToJSON, FromJSON, Generic, Eq, Show)
  
--Types saved in dB
data UserDB = UserDB
  { userId        :: String
  } deriving (ToJSON, FromJSON, Generic, Eq, Show)
  
data RepoDB = RepoDB
  { repoName      :: String
  , repoOwner     :: String
  , repoSize      :: Int
  } deriving (ToJSON, FromJSON, Generic, Eq, Show)
  
data LinkDB = LinkDB
  { linkType      :: Int
  , linkName      :: String
  , source        :: String
  , destination   :: String
  } deriving (ToJSON, FromJSON, Generic, Eq, Show)
  
userRepo :: Int
userRepo = 1
userUser :: Int
userUser = 2
repoUser :: Int
repoUser = 3

owns = "Owns"
contributesTo = "ContributesTo"

n4password = "neo4J"
n4user = "neo4j"
  
--Queries
--Searches
allUsers :: IO[UserDB]
allUsers = do
   pipe <- connect $ def { user = n4user, password = n4password }
   result <- run pipe $ query "MATCH (n:User)  RETURN n"
   close pipe
   putStrLn $ show result
   x <- mapM toUser result
   return x
   
toUser :: Record-> IO UserDB
toUser record = do
    T name <- record `at` "userName"
    putStrLn $ show record
    return $ UserDB (unpack name)
   
allRepo :: IO[RepoDB]
allRepo = do
   pipe <- connect $ def { user = n4user, password = n4password }
   result <- run pipe $ query "MATCH (n:Repo)  RETURN n"
   close pipe
   putStrLn $ show result
   x <- mapM toRepo result
   return x
   
toRepo :: Record-> IO RepoDB
toRepo record = do    
    T name <- record `at` "repoName"
    T owner <- record `at` "repoOwner"
    I rSize <- record `at` "repoSize"
    putStrLn $ show record
    return $ RepoDB (unpack name) (unpack owner) rSize
   
-- allLinks :: IO [LinkDB]
-- allLinks = do
   -- pipe <- connect $ def { user = n4user, password = n4password }
   -- --not quite ideal yet
   -- result <- run pipe $ query "MATCH (s) OPTIONAL MATCH (n)-[r]-(d) RETURN s,r,d"
   -- close pipe
   -- putStrLn $ show result
   -- x <- mapM toLink result
   -- return x

-- toLink :: Record-> IO LinkDB   
-- toLink record = do    
    -- L linkParts <- record `at` "_fields"
    -- T sId <- list[0] `at` "userId"
    -- T linkType <- list[1] `at` "type"
    -- T dId <- list[2] `at` "repoId"
    -- let x =  unpack linkType
    -- lt <- digitToInt x[0]
    -- putStrLn $ show record
    -- return $ LinkDB (lt) (unpack sId) $ unpack dId
makeLink :: Int -> String -> UserDB -> RepoDB -> IO LinkDB
makeLink linkType linkName user repository =  do
    putStrLn "madelink"
    let link = LinkDB linkType linkName (userId user) (repoName repository)
    return link
    
makeLink2 :: Int -> String -> RepoDB -> UserDB -> IO LinkDB
makeLink2 linkType linkName repository user=  do
    let link = LinkDB linkType linkName (userId user) (repoName repository)
    return link
      
--Empty
clearDB :: IO String
clearDB = do
   pipe <- connect $ def { user = n4user, password = n4password }
   result <- run pipe $ query "MATCH (n) OPTIONAL MATCH (n)-[r]-() DELETE n,r"
   close pipe
   putStrLn $ show result
   let r = show result
   return r
   
--Add

addUser :: UserDB -> IO String
addUser newUser = do
   pipe <- connect $ def { user = n4user, password = n4password }
   result <- run pipe $ queryP "MERGE (n:User {userId: {id}})" 
                               (fromList [("id", T (fromString (userId newUser)))])
   close pipe
   putStrLn $ show result
   let r = show result
   return r
   
addRepo :: RepoDB -> IO()
addRepo newRepo = do
   pipe <- connect $ def { user = n4user, password = n4password }
   result <- run pipe $ queryP "MERGE (n:Repo {repoName: {name}, repoOwner: {owner} }) ON MATCH SET n.repoSize = {size} ON CREATE SET n.repoSize = {size}" 
                               (fromList [("name", T (fromString (repoName newRepo))), ("owner", T (fromString (repoOwner newRepo))), ("size", I (repoSize newRepo))])
   close pipe
   putStrLn $ show result

   
addLink :: LinkDB -> IO String
addLink newLink = do
   pipe <- connect $ def { user = n4user, password = n4password }
   result <- run pipe $ query $ Data.Text.pack $ linkRequest newLink
   close pipe
   putStrLn $ show result
   let r = show result
   return r
      
linkRequest :: LinkDB -> String  
linkRequest newLink
   | (lt == userRepo) = "MATCH  (s:User {userId: \"" ++ (source newLink) ++ "\"}) MATCH  (d:Repo {repoName: \""++ (destination newLink) ++ "\"}) MERGE (s)-[o:" ++ (linkName newLink) ++ "]->(d)"
   | (lt == userUser) = "MATCH  (s:User {userId: \"" ++ (source newLink) ++ "\"}) MATCH  (d:User {userId: \""++ (destination newLink)  ++ "\") MERGE (s)-[o:\"" ++ (linkName newLink) ++ "\"]->(d)"
   | otherwise        = "MATCH  (d:User {userId: \"" ++ (source newLink) ++ "\"}) MATCH  (s:Repo {repoName: \""++ (destination newLink)  ++ "\") MERGE (s)-[o:\"" ++ (linkName newLink) ++ "\"]->(d)"
   where lt = linkType newLink
   
addAuth :: String -> IO()
addAuth auth = do
   pipe <- connect $ def { user = n4user, password = n4password }
   result <- run pipe $ query "MATCH (r:Auth) DELETE r" 
   result <- run pipe $ queryP "CREATE (n:Auth {token: {a}})" 
                               (fromList [("a", T (fromString auth))])
   
   putStrLn $ show result
   close pipe

getAuth :: IO(Text)
getAuth = do
   pipe <- connect $ def { user = n4user, password = n4password }
   result <- run pipe $ query "MATCH (n:Auth) RETURN n.token as token"
   x <- mapM toAuth result
   close pipe
   return $ Prelude.head x

toAuth :: Record-> IO Text
toAuth record = do
    putStrLn $ show record
    T token <- record `at` "token"
    putStrLn $ unpack token
    return token