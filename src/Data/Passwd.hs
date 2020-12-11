{-# LANGUAGE OverloadedStrings #-}
module Data.Passwd where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Text as T
import Text.Printf

--
standardShells :: [T.Text]
standardShells = ["/usr/sbin/nologin", "/bin/false"]

-- PasswdEntry
data PasswdEntry = PasswdEntry { username, password :: T.Text, uid, gid :: Int, comment, home, shell :: T.Text} deriving (Eq)

instance Show PasswdEntry where
    show (PasswdEntry u p uid gid c h s) = printf "%s:%s:%d:%d:%s:%s:%s" u p uid gid c h s

parsePasswdEntry' :: Parser PasswdEntry
parsePasswdEntry' = do
    let tillColon = takeTill ((==) ':')
    username <- tillColon
    char ':'
    passwd <- tillColon
    char ':'
    uid <- decimal
    char ':'
    guid <- decimal
    char ':'
    comment <- tillColon
    char ':'
    home <- tillColon
    char ':'
    shell <- takeTill isEndOfLine

    return $ PasswdEntry username passwd uid guid comment home shell

parsePasswdEntry :: T.Text -> Either String PasswdEntry
parsePasswdEntry = parseOnly parsePasswdEntry'

-- Passwd
type Passwd = [PasswdEntry]

showPasswd :: Passwd -> String
showPasswd xs = concat $ map (\x -> show x++"\n") xs

passwdParser' :: Parser Passwd
passwdParser' = many $ parsePasswdEntry' <* endOfLine

parseLocalPasswd :: IO (Either String Passwd)
parseLocalPasswd = do
    file <- readFile "/etc/passwd"
    return $ parsePasswd $ T.pack file

parsePasswd :: T.Text -> Either String Passwd
parsePasswd = parseOnly passwdParser'

-- Passwd utils
findUIDGIDZero :: Passwd -> Passwd
findUIDGIDZero = filter (\x -> uid x == 0 || gid x == 0)

findNonstandardShell :: Passwd -> Passwd
findNonstandardShell = filter (\x -> not $ shell x `elem` standardShells)
