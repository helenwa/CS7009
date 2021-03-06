{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ExtendedDefaultRules   #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}

module N4JHelper where

import Database.Bolt hiding(unpack)
import Data.List
import Data.Aeson
import GHC.Generics

import DBHelper
import GithubHelper


--Force direct Graph
--objects
data FDG = FDG{
  nodes :: [FDGnode]
, links :: [FDGlink]
} deriving (ToJSON, FromJSON, Generic, Eq, Show)

data FDGnode = FDGnode{
  id :: String
, group :: Integer
} deriving (ToJSON, FromJSON, Generic, Eq, Show)

data FDGlink = FDGlink{
  source :: String
, target :: String
, value :: Integer
} deriving (ToJSON, FromJSON, Generic, Eq, Show)
--create
usersToNodes :: [UserDB] -> [FDGnode]
usersToNodes users = do
     x <- map userToNode users
     return x

userToNode :: UserDB -> FDGnode
userToNode u = ( FDGnode (userId u) 1)

reposToNodes :: [RepoDB] -> [FDGnode]
reposToNodes repos = do
    x <-  map repoToNode repos
    return x

repoToNode :: RepoDB -> FDGnode
repoToNode r = FDGnode (repoName r) 6

formatLinks :: [LinkDB] -> [FDGlink]
formatLinks links = do
    x <- map formatLink links
    return x

formatLink :: LinkDB -> FDGlink
formatLink l = FDGlink (DBHelper.source l) (destination l) (toInteger (linkType l))

fullFDG :: [UserDB] -> [RepoDB] -> [LinkDB] -> FDG
fullFDG users repos links = FDG ((usersToNodes users) ++ (reposToNodes repos)) (formatLinks links)

--Language List
data LangList = LangList{
  allTime :: [Language]
, recent :: [Language]
, known :: [Language]
} deriving (ToJSON, FromJSON, Generic, Eq, Show)

data Pie = Pie
  { language :: String
  , view :: String
  , count :: Int
  } deriving (ToJSON, FromJSON, Generic, Eq, Show)
  
formatToPie :: LangList -> IO[Pie]
formatToPie lists = do
    let a = map (langToPie "allTime") (allTime lists)
    let b = map (langToPie "recent") (N4JHelper.recent lists)
    let c = map (langToPie "Your") (known lists)
    let d = a ++ b
    putStrLn $ show d
    let e = d ++ c
    putStrLn $ show e
    return e
    
langToPie :: String -> Language -> Pie
langToPie v l = Pie (name l) v (fromEnum (DBHelper.value l))