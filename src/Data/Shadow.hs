{-# LANGUAGE OverloadedStrings #-}
module Data.Shadow where


import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Text as T
import Text.Printf

standardShells :: [T.Text]
standardShells = ["/usr/sbin/nologin", "/bin/false"]

-- ShadowEntry
data ShadowEntry = ShadowEntry { username, hash, lastChanged :: T.Text } deriving (Eq)

instance Show ShadowEntry where
    show (ShadowEntry u h l) = printf "%s:%s:%s" u h l

parseShadowEntry' :: Parser ShadowEntry
parseShadowEntry' = do
    let tillColon = takeTill ((==) ':')
    username <- tillColon
    char ':'
    hash <- tillColon
    char ':'
    lastChanged <- tillColon
    takeTill isEndOfLine

    return $ ShadowEntry username hash lastChanged

parseShadowEntry :: T.Text -> Either String ShadowEntry
parseShadowEntry = parseOnly parseShadowEntry'

-- Shadow
type Shadow = [ShadowEntry]

showShadow :: Shadow -> String
showShadow xs = concat $ map (\x -> show x++"\n") xs

passwdParser' :: Parser Shadow
passwdParser' = many $ parseShadowEntry' <* endOfLine

parseLocalShadow = do
    file <- readFile "/etc/shadow"
    return $ parseShadow $ T.pack file

parseShadow :: T.Text -> Either String Shadow
parseShadow = parseOnly passwdParser'

-- Shadow utils
usersWithHash :: Shadow -> Shadow
usersWithHash = filter (\x -> hash x /= "!" && hash x /= "*" && hash x /= "!!")
