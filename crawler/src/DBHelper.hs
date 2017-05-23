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

data ListOb = ListOb
 { users :: [UserDB]
 , repos ::[RepoDB]
 , links :: [LinkDB]
 } deriving (ToJSON, FromJSON, Generic, Eq, Show)
 
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
  , recent        :: Bool
  , repoLanguage  :: String
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
   result <- run pipe $ query "MATCH (n:User)  RETURN n.userId"
   
   putStrLn $ show result
   x <- mapM toUser result
   close pipe
   return x
   
toUser :: Record-> IO UserDB
toUser record = do
    T name <- record `at` "n.userId"
    putStrLn $ show record
    return $ UserDB (unpack name)
   
allRepo :: IO[RepoDB]
allRepo = do
   pipe <- connect $ def { user = n4user, password = n4password }
   result <- run pipe $ query "MATCH (n:Repo)  RETURN n.repoName as repoName, n.repoOwner as repoOwner, n.repoSize as repoSize, n.repoLanguage as repoLanguage, n.recent as recent"
   
   putStrLn $ show result
   x <- mapM toRepo result
   close pipe
   return x
   
toRepo :: Record-> IO RepoDB
toRepo record = do    
    T name <- record `at` "repoName"
    T owner <- record `at` "repoOwner"
    I rSize <- record `at` "repoSize"
    T lang <- record `at` "repoLanguage"
    B recent <- record `at` "recent"
    putStrLn $ show record
    return $ RepoDB (unpack name) (unpack owner) rSize recent (unpack lang)
   
allLinks :: IO [LinkDB]
allLinks = do
   pipe <- connect $ def { user = n4user, password = n4password }
   --not quite ideal yet
   result <- run pipe $ query "MATCH (s:User) OPTIONAL MATCH (s)-[r]-(d) RETURN s.userId as source, d.repoName as destination"
   
   putStrLn $ show result
   x <- mapM toLink result
   close pipe
   return x

toLink :: Record-> IO LinkDB   
toLink record = do   
    T s <- record `at` "source"
    T d <- record `at` "destination"
    let link = LinkDB 1 ("Type") (unpack s) (unpack d) 
    return link
    
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
   result <- run pipe $ queryP "MERGE (n:Repo {repoName: {name}, repoOwner: {owner} }) ON MATCH SET n.repoSize = {size}, n.repoLanguage = {lang}, n.recent = {r}  ON CREATE SET n.repoSize = {size}, n.repoLanguage = {lang}, n.recent = {r}" 
                               (fromList [("name", T (fromString (repoName newRepo))), ("owner", T (fromString (repoOwner newRepo))), ("size", I (repoSize newRepo)),("lang", T (fromString (repoLanguage newRepo))),("r", B (recent newRepo))])
   addUser $ UserDB $ repoOwner newRepo 
   resultOwns <- run pipe $ queryP "MATCH  (s:User {userId: {owner}}) MATCH  (d:Repo {repoName: {name}}) MERGE (s)-[o:Owns]->(d)" 
                               (fromList [("name", T (fromString (repoName newRepo))), ("owner", T (fromString (repoOwner newRepo)))])                         
   addLanguage $repoLanguage newRepo
   putStrLn $ show result
   close pipe
   
addLanguage:: String ->IO()
addLanguage l = do
   pipe <- connect $ def { user = n4user, password = n4password }
   result <- run pipe $ queryP "MERGE (n:Language {name: {l}})" 
                               (fromList [("l", T (fromString l))])
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