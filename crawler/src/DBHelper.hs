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
    T id <- record `at` "repoId"
    T label <- record `at` "repolabel"
    putStrLn $ show record
    return $ RepoDB (unpack id) $ unpack label
   
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
   