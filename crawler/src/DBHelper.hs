{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ExtendedDefaultRules   #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}

module DBHelper where

import Database.Bolt
import Data.Text
import Data.Map
import Data.Aeson
import GHC.Generics
import Data.String

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
testFunction :: IO ()
testFunction = do
   pipe <- connect $ def { user = n4user, password = n4password }
   result <- run pipe $ query "MATCH (n:User) WHERE n.name CONTAINS \"Bob\" RETURN n"
   close pipe
   putStrLn $ show result
   
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
   result <- run pipe $ queryP "CREATE (n:User {userId: {id}, hops: {h}}) RETURN n" 
                               (fromList [("id", T (fromString (userId newUser))), ("h", I (hops newUser))])
   close pipe
   putStrLn $ show result
   let r = show result
   return r
   
addRepo :: RepoDB -> IO String
addRepo newRepo = do
   pipe <- connect $ def { user = n4user, password = n4password }
   result <- run pipe $ queryP "CREATE (n:Repo {repoId: {id}, repolabel: {label}) RETURN n" 
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
   | (lt == userRepo) = "MATCH  (s:User {userId: " ++ (source newLink) ++ "}) MATCH  (d:Repo {repoId: "++ (destination newLink) ++ ") CREATE (s)-[o:" ++ (linkName newLink) ++ "]->(d) RETURN o"
   | (lt == userUser) = "MATCH  (s:User {userId: " ++ (source newLink) ++ "}) MATCH  (d:User {userId: "++ (destination newLink)  ++ ") CREATE (s)-[o:" ++ (linkName newLink) ++ "]->(d) RETURN o"
   | otherwise        = "MATCH  (d:User {userId: " ++ (source newLink) ++ "}) MATCH  (s:Repo {repoId: "++ (destination newLink)  ++ ") CREATE (s)-[o:" ++ (linkName newLink) ++ "]->(d) RETURN o"
   where lt = linkType newLink
   