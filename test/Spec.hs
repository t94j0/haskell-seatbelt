{-# LANGUAGE OverloadedStrings #-}   

import Test.Hspec
import Control.Exception (evaluate)
import Data.Shadow
import Data.Passwd

main :: IO ()
main = hspec $ do
  describe "Shadow" $ do
    it "parses /etc/shadow file" $ do
        let tgt = "root:!:18568:0:99999:7:::\ndaemon:*:18474:0:99999:7:::\n"
        case parseShadow tgt of
          Left x -> expectationFailure x
          Right x -> x `shouldBe` [ShadowEntry "root" "!" "18568", ShadowEntry "daemon" "*" "18474"]

  describe "Passwd" $ do
    it "parses passwd entry" $ do
        case parsePasswdEntry "root:x:0:0:root:/root:/bin/bash" of
          Left _ -> expectationFailure "should parse"
          Right x -> x `shouldBe` (PasswdEntry "root" "x" 0 0 "root" "/root" "/bin/bash")

    it "parses /etc/passwd file" $ do
        let inp = "root:x:0:0:root:/root:/bin/bash\ndaemon:x:1:1:daemon:/usr/sbin:/usr/sbin/nologin\n"
        let tgt = [PasswdEntry "root" "x" 0 0 "root" "/root" "/bin/bash", PasswdEntry "daemon" "x" 1 1 "daemon" "/usr/sbin" "/usr/sbin/nologin"]
        case parsePasswd inp of
          Left _ -> expectationFailure "should parse"
          Right x -> x `shouldBe` tgt

    it "finds UID == 0 or GUID == 0" $ do
        let inp = [PasswdEntry "root" "x" 0 0 "root" "/root" "/bin/bash", PasswdEntry "daemon" "x" 1 1 "daemon" "/usr/sbin" "/usr/sbin/nologin"]
        case findGUIDZero inp of
          x:[] -> Data.Passwd.username x `shouldBe` "root"
          _ -> expectationFailure "not root"

    it "finds nonstandard shells" $ do
        let inp = [PasswdEntry "root" "x" 0 0 "root" "/root" "/bin/bash", PasswdEntry "daemon" "x" 1 1 "daemon" "/usr/sbin" "/usr/sbin/nologin"]
        case findNonstandardShell inp of
          x:[] -> Data.Passwd.username x `shouldBe` "root"
          _ -> expectationFailure "not root"
