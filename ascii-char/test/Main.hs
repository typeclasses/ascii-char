module Main (main) where

import ASCII.Char

import Data.Function (($))
import Data.List (length)
import Data.Maybe (Maybe (..))
import Prelude (maxBound, minBound)
import System.IO (IO)
import Test.Hspec (hspec, it, shouldBe)

main :: IO ()
main = hspec $ do

    it "toInt" $ do
        let f = toInt
        f Null `shouldBe` 0
        f CapitalLetterA `shouldBe` 65
        f SmallLetterA `shouldBe` 97
        f Delete `shouldBe` 127

    it "fromIntMaybe" $ do
        let f = fromIntMaybe
        f (-1) `shouldBe` Nothing
        f 0 `shouldBe` Just Null
        f 65 `shouldBe` Just CapitalLetterA
        f 127 `shouldBe` Just Delete
        f 128 `shouldBe` Nothing

    it "fromIntUnsafe" $ do
        let f = fromIntUnsafe
        f 65 `shouldBe` CapitalLetterA
        f 66 `shouldBe` CapitalLetterB
        f 67 `shouldBe` CapitalLetterC

    it "allCharacters" $ do
        length allCharacters `shouldBe` 128

    it "Bounded" $ do
        minBound `shouldBe` Null
        maxBound `shouldBe` Delete
