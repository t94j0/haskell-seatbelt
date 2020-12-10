module Data.Shadow where

import System.IO  
import Data.List
import Data.List.Split
import Text.Printf

data Shadow = Shadow {
  users :: [ShadowEntry]
} deriving (Show)

parseShadowString :: String -> Shadow
parseShadowString shadow = Shadow $ map parseShadowEntryString (words shadow)

parseShadowFile :: String -> IO Shadow
parseShadowFile fileName  = do
  content <- readFile fileName
  return $ parseShadowString content

parseShadowLocal :: IO Shadow
parseShadowLocal = parseShadowFile "/etc/shadow"

getUsers :: Shadow -> [String]
getUsers shadow = [ username s | s <- (users shadow) ]

hasName :: String -> ShadowEntry -> Bool
hasName target user = (username user) == target

getUser :: Shadow -> String -> Maybe ShadowEntry
getUser shadow username = find (hasName username) (users shadow)


data ShadowEntry = ShadowEntry {
  username :: String,
  hash :: String,
  dateCreated :: String
}

instance Show ShadowEntry where
    show (ShadowEntry u h d) = printf "%s %s %s" u h d

stringToInt :: String -> Integer
stringToInt x = read x :: Integer

getValue :: String -> Int -> String
getValue x y = splitOn ":" x !! y

parseShadowEntryString :: String -> ShadowEntry
parseShadowEntryString x = ShadowEntry (getValue x 0) (getValue x 1) (getValue x 2)

