{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module DBHelper where

import Database.Bolt
import Data.Text
import Data.Map

testFunction :: IO ()
testFunction = do
   pipe <- connect $ def { user = "neo4j", password = "neo4j" }
   result <- run pipe $ query "MATCH (n:User) WHERE n.name CONTAINS \"Bob\" RETURN n"
   close pipe
   putStrLn $ show result
   
testFunction' :: Text -> IO () 
testFunction' name = do
   pipe <- connect $ def { user = "neo4j", password = "neo4j" }
   result <- run pipe $ query $ Data.Text.pack $ "MATCH (n:User) WHERE n.name CONTAINS \"" ++ Data.Text.unpack name ++ "\" RETURN n"
   close pipe
   putStrLn $ show result

testFunction'' :: Text -> IO () 
testFunction'' username = do
   pipe <- connect $ def { user = "neo4j", password = "neo4j" }
   result <- run pipe $ queryP "MATCH (n:User) WHERE n.name CONTAINS {name} RETURN n" 
                               (fromList [("name", T username)])
   putStrLn $ show result