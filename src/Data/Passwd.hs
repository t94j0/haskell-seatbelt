{-# LANGUAGE OverloadedStrings #-}
module Data.Passwd where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Text as T

--
standardShells :: [T.Text]
standardShells = ["/usr/sbin/nologin", "/bin/false"]

-- PasswdEntry
data PasswdEntry = PasswdEntry { username, password :: T.Text, uid, gid :: Int, comment, home, shell :: T.Text} deriving (Eq, Show)

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

passwdParser' :: Parser Passwd
passwdParser' = many $ parsePasswdEntry' <* endOfLine

parseLocalPasswd = do
    file <- readFile "/etc/passwd"
    return $ parsePasswd $ T.pack file

parsePasswd :: T.Text -> Either String Passwd
parsePasswd = parseOnly passwdParser'

-- Passwd utils
findGUIDZero :: Passwd -> Passwd
findGUIDZero = filter (\x -> uid x == 0 || gid x == 0)

findNonstandardShell :: Passwd -> Passwd
findNonstandardShell xs = filter (\x -> not $ shell x `elem` standardShells) xs
