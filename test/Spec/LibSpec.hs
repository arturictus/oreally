{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Spec.LibSpec where
import Lib
import Test.Hspec
import Data.Maybe
import qualified System.Environment as E

spec :: Spec
spec = do
  describe "Lib" $ do
    it "parseURI" $ do
      parseURI "https://learning.oreilly.com/library/view/get-programming-with/9781617293764/" `shouldBe` Just ("get-programming-with", "9781617293764")
      parseURI "https://learning.oreilly.com/" `shouldBe` Nothing
      parseURI "" `shouldBe` Nothing
    -- it "getAuth" $ do
    --   getAuth "NOP" `shouldBe` Nothing
    --   E.setEnv "MY_VAR" "a:b"
    --   getAuth "MY_VAR" `shouldBe` Just ("a", "b")
    it "buildAuth" $ do
      buildAuth (Just "NOP") `shouldBe` Nothing
      buildAuth (Just "a:b") `shouldBe` Just ("a", "b")
    it "main" $ do
      E.setEnv "MY_VAR" "a:b"
      main "MY_VAR" "https://learning.oreilly.com/library/view/get-programming-with/9781617293764/"
      main "NOP" ""


