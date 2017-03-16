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
  } deriving (ToJSON, FromJSON, Generic, Eq, Show)

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
   
   
--Add

addUser :: UserDB -> IO String
addUser newUser = do
   pipe <- connect $ def { user = "neo4j", password = "neo4J" }
   result <- run pipe $ queryP "CREATE (n:User {userId: {id}, hops: 7}) RETURN n" 
                               (fromList [("id", T (fromString (userId newUser)))])
   close pipe
   putStrLn $ show result
   let r = show result
   return r