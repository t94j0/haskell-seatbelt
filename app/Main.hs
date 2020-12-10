module Main where

import System.Posix.User

-- modules
printSudoers = readFile "/etc/sudoers" >>= putStrLn

printPasswd = readFile "/etc/passwd" >>= putStrLn

-- modes
rootMode = do
    printSudoers
    printPasswd

userMode = do
    printPasswd

main :: IO ()
main = do
    uid <- getEffectiveUserID
    if uid == 0
       then rootMode
       else userMode
