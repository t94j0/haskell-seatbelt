module Main where

import Data.Passwd
import Data.Shadow
import System.Console.Pretty
import System.Posix.User
import System.Process

-- utils
title = color Red

-- modules
printSudoers = do
    shadow <- parseLocalShadow
    case shadow of
      Left x -> putStrLn $ "error: "++x
      Right pwd -> do
          putStrLn $ title "Users with hash"
          putStrLn $ showShadow $ usersWithHash pwd

printPasswd = do
    users <- parseLocalPasswd
    case users of
      Left x -> putStrLn $ "error: "++x
      Right pwd -> do
          putStrLn $ title "Zero in UID or GUID:"
          putStrLn $ showPasswd $ findUIDGIDZero pwd
          putStrLn $ title "Nonstandard shells:"
          putStrLn $ showPasswd $ findNonstandardShell pwd

who = do
    putStrLn $ title "Users logged in:"
    who <- readProcess "w" [] []
    putStrLn who

-- modes
rootMode = do
    printSudoers
    printPasswd
    who

userMode = do
    printPasswd
    who

main :: IO ()
main = do
    uid <- getEffectiveUserID
    if uid == 0
       then rootMode
       else userMode
