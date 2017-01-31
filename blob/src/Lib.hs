module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
  o <- doMyOutput "Stephen" 4
  putStrLn o
  
doMyOutput :: String -> Int -> IO String
doMyOutput msg count = do
  putStrLn "sneaky"
  return $ "My name's first " ++ show count ++ " letters are " ++ take count msg