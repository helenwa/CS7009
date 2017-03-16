
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE RecursiveDo #-}



{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}



import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Text                    (pack, unpack)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Database.MongoDB
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Logger
import           Prelude        Data.Bson.Generic

module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Message = Message { name    :: String
                       , message :: String
                       } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

deriving instance FromBSON String  -- we need these as BSON does not provide
deriving instance ToBSON   String

storeMessage :: Message -> Handler Bool
storeMessage msg@(Message key _) = liftIO $ do
    warnLog $ "Storing message under key " ++ key ++ "."
    withMongoDbConnection $ upsert (select ["name" =: key] "MESSAGE_RECORD") $ toBSON msg
    return True

searchMessage :: Maybe String -> Handler [Message]
searchMessage (Just key) = liftIO $ do
    warnLog $ "Searching for value for key: " ++ key
    docs <- find (select ["name" =: key] "MESSAGE_RECORD") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Message) docs

searchMessage Nothing = liftIO $ do
    warnLog $ "No key for searching."
    return $ ([] :: [Message])
    
withMongoDbConnection :: Action IO a -> IO a
withMongoDbConnection act  = do
  ip <- mongoDbIp
  port <- mongoDbPort
  database <- mongoDbDatabase
  pipe <- connect (host ip)
  ret <- runResourceT $ liftIO $ access pipe master (pack database) act
  close pipe
  return ret