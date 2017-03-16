{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric   #-}

module DBHelper where

import Database.Bolt
import Data.Text
import Data.Map
import Data.Aeson
import GHC.Generics

--Types saved in dB

data UserDB = UserDB
  { hops          :: Int
  , userId        :: String
  } deriving (ToJSON, FromJSON, Generic, Eq, Show)
  
data RepoDB = RepoDB
  { hops          :: Int
  , repoId        :: String
  , repolabel     :: String
  } deriving (ToJSON, FromJSON, Generic, Eq, Show)
  
data linkDB = linkDB
  { linkType      :: Int
  , source        :: String
  , destination   :: String
  } deriving (ToJSON, FromJSON, Generic, Eq, Show)


  
--Queries
--Searches
testFunction :: IO ()
testFunction = do
   pipe <- connect $ def { user = "neo4j", password = "neo4J" }
   result <- run pipe $ query "MATCH (n:User) WHERE n.name CONTAINS \"Bob\" RETURN n"
   close pipe
   putStrLn $ show result
   
testFunction' :: Text -> IO () 
testFunction' name = do
   pipe <- connect $ def { user = "neo4j", password = "neo4J" }
   result <- run pipe $ query $ Data.Text.pack $ "MATCH (n:User) WHERE n.name CONTAINS \"" ++ Data.Text.unpack name ++ "\" RETURN n"
   close pipe
   putStrLn $ show result

testFunction'' :: Text -> IO () 
testFunction'' username = do
   pipe <- connect $ def { user = "neo4j", password = "neo4J" }
   result <- run pipe $ queryP "MATCH (n:User) WHERE n.name CONTAINS {name} RETURN n" 
                               (fromList [("name", T username)])
   putStrLn $ show result
   
   
--Add