{-# LANGUAGE OverloadedStrings #-}

-- file: HCsvSpec.hs
-- author: Jacob Xie
-- date: 2025/09/18 13:59:53 Thursday
-- brief:

module HCsvSpec (spec) where

import Test.Hspec
import qualified Data.Vector as V
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStr, hClose)
import HCsv

-- Sample data type for testing
data Person = Person
  { name :: String
  , age :: Int
  } deriving (Show, Eq)

instance FromRecord Person where
  parseRecord fi rec = do
    name' <- (fi, "name") ~> rec
    age'  <- (fi, "age")  ~> rec
    return $ Person name' age'

spec :: Spec
spec = do
  describe "CSV Reading" $ do
    it "should read raw CSV data correctly" $ do
      withSystemTempFile "test.csv" $ \path handle -> do
        let csvContent = "name,age\nJohn,30\nJane,25"
        hPutStr handle csvContent
        hClose handle

        result <- readCsvRaw path
        case result of
          Right csv -> do
            V.length csv `shouldBe` 3  -- header + 2 rows
            (csv V.! 0) V.! 0 `shouldBe` "name"
            (csv V.! 1) V.! 1 `shouldBe` "30"
          Left err -> expectationFailure $ "Failed to parse CSV: " ++ err

    it "should parse CSV into data type" $ do
      withSystemTempFile "test.csv" $ \path handle -> do
        let csvContent = "name,age\nJohn,30\nJane,25"
        hPutStr handle csvContent
        hClose handle

        result <- readCsv path
        case result of
          Right persons -> do
            V.length persons `shouldBe` 2
            persons `shouldBe` V.fromList [Person "John" 30, Person "Jane" 25]
          Left err -> expectationFailure $ "Failed to parse CSV: " ++ err
