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
  { hops          :: Int
  , userId        :: String
  } deriving (ToJSON, FromJSON, Generic, Eq, Show)
  
data RepoDB = RepoDB
  { repoId        :: String
  , repolabel     :: String
  } deriving (ToJSON, FromJSON, Generic, Eq, Show)
  
data LinkDB = LinkDB
  { linkType      :: Int
  , source        :: String
  , destination   :: String
  , linkName      :: String
  } deriving (ToJSON, FromJSON, Generic, Eq, Show)
  
userRepo :: Int
userRepo = 1
userUser :: Int
userUser = 2
repoUser :: Int
repoUser = 3

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
    T name <- record `at` "name"
    I h <- record `at` "hops"
    putStrLn $ show record
    return $ UserDB h $ unpack name
   
-- allRepo :: IO [RepoDB]
-- allRepo = do
   -- pipe <- connect $ def { user = n4user, password = n4password }
   -- result <- run pipe $ query "MATCH (n:Repo)  RETURN n"
   -- close pipe
   -- putStrLn $ show result
   -- return result
   
-- allLinks :: IO [RepoDB]
-- allLinks = do
   -- pipe <- connect $ def { user = n4user, password = n4password }
   -- --not quite ideal yet
   -- result <- run pipe $ query "MATCH (s) OPTIONAL MATCH (n)-[r]-(d) RETURN s,r,d"
   -- close pipe
   -- putStrLn $ show result
   -- return result
   
testFunction' :: Text -> IO () 
testFunction' name = do
   pipe <- connect $ def { user = n4user, password = n4password }
   result <- run pipe $ query $ Data.Text.pack $ "MATCH (n:User) WHERE n.name CONTAINS \"" ++ Data.Text.unpack name ++ "\" RETURN n"
   close pipe
   putStrLn $ show result

testFunction'' :: Text -> IO () 
testFunction'' username = do
   pipe <- connect $ def { user = n4user, password = n4password }
   result <- run pipe $ queryP "MATCH (n:User) WHERE n.name CONTAINS {name} RETURN n" 
                               (fromList [("name", T username)])
   close pipe
   putStrLn $ show result
   
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
   result <- run pipe $ queryP "MERGE (n:User {userId: {id}}) ON MATCH SET n.hops = {h} ON CREATE SET n.hops = {h} RETURN n" 
                               (fromList [("id", T (fromString (userId newUser))), ("h", I (hops newUser))])
   close pipe
   putStrLn $ show result
   let r = show result
   return r
   
addRepo :: RepoDB -> IO String
addRepo newRepo = do
   pipe <- connect $ def { user = n4user, password = n4password }
   result <- run pipe $ queryP "MERGE (n:Repo {repoId: {id}}) ON MATCH SET n.repolabel = {label} ON CREATE SET n.repolabel = {label} RETURN n" 
                               (fromList [("id", T (fromString (repoId newRepo))), ("id", T (fromString (repolabel newRepo)))])
   close pipe
   putStrLn $ show result
   let r = show result
   return r
   
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
   | (lt == userRepo) = "MATCH  (s:User {userId: " ++ (source newLink) ++ "}) MATCH  (d:Repo {repoId: "++ (destination newLink) ++ ") MERGE (s)-[o:" ++ (linkName newLink) ++ "]->(d) RETURN o"
   | (lt == userUser) = "MATCH  (s:User {userId: " ++ (source newLink) ++ "}) MATCH  (d:User {userId: "++ (destination newLink)  ++ ") MERGE (s)-[o:" ++ (linkName newLink) ++ "]->(d) RETURN o"
   | otherwise        = "MATCH  (d:User {userId: " ++ (source newLink) ++ "}) MATCH  (s:Repo {repoId: "++ (destination newLink)  ++ ") MERGE (s)-[o:" ++ (linkName newLink) ++ "]->(d) RETURN o"
   where lt = linkType newLink
   