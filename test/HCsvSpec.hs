{-# LANGUAGE OverloadedStrings #-}

-- file: HCsvSpec.hs
-- author: Jacob Xie
-- date: 2025/09/18 13:59:53 Thursday
-- brief:

module HCsvSpec (spec) where

import Control.Exception (SomeException, try)
import Control.Monad (replicateM_)
import qualified Data.Attoparsec.Lazy as AL
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Either (isLeft)
import qualified Data.Vector as V
import HCsv
import HCsv.Parser (csvWithHeader)
import System.Directory (listDirectory)
import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec

-- Sample data type for testing
data Person = Person
  { name :: String,
    age :: Int
  }
  deriving (Show, Eq)

instance FromRecord Person where
  parseRecord fi rec = do
    name' <- (fi, "name") ~> rec
    age' <- (fi, "age") ~> rec
    return $ Person name' age'

instance ToRecord Person where
  toNamedFields p =
    [ ("name", toField (name p)),
      ("age", toField (age p))
    ]

-- | Integration specs covering read/encode helpers.  Each example uses
-- temporary files so the tests stay isolated and mirrors real IO
-- usage.
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
            V.length csv `shouldBe` 3 -- header + 2 rows
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

    it "should return a parse error for empty CSV files" $ do
      withSystemTempFile "empty.csv" $ \path handle -> do
        hPutStr handle ""
        hClose handle
        outcome <- try (readCsv path) :: IO (Either SomeException (Either String (V.Vector Person)))
        case outcome of
          Right res -> res `shouldSatisfy` isLeft
          Left _ -> expectationFailure "readCsv threw an exception instead of returning Left"

    it "should close file handles after reading" $ do
      let countFds = length <$> listDirectory "/proc/self/fd"
      withSystemTempFile "handles.csv" $ \path handle -> do
        hPutStr handle "name,age\nJohn,30\nJane,25"
        hClose handle
        fdBefore <- countFds
        replicateM_ 50 $ readCsvRaw path
        fdAfter <- countFds
        (fdAfter - fdBefore) `shouldSatisfy` (<= 5)

    it "should fail parsing when numeric fields are invalid" $ do
      withSystemTempFile "invalid.csv" $ \path handle -> do
        let csvContent = "name,age\nJohn,abc"
        hPutStr handle csvContent
        hClose handle
        res <- readCsv path :: IO (Either String (V.Vector Person))
        res `shouldSatisfy` isLeft

    it "should reject rows that do not match header length" $ do
      let csvContent = "name,age\nJohn\nJane,25,Engineer"
      let parsed = AL.parseOnly csvWithHeader (BL.pack csvContent)
      parsed `shouldSatisfy` isLeft

  describe "CSV Writing" $ do
    it "should write and read CSV in a round trip" $ do
      let persons = V.fromList [Person "John" 30, Person "Jane" 25]
      withSystemTempFile "roundtrip.csv" $ \path handle -> do
        hClose handle
        res <- writeCsv path persons
        res `shouldBe` Right ()
        back <- readCsv path
        back `shouldBe` Right persons

    it "should escape commas and quotes when writing" $ do
      let persons = V.fromList [Person "Smith, Jr." 40, Person "Ann \"Quote\"" 28]
      withSystemTempFile "escape.csv" $ \path handle -> do
        hClose handle
        res <- writeCsv path persons
        res `shouldBe` Right ()
        back <- readCsv path
        back `shouldBe` Right persons

    it "should reject encoding empty inputs" $ do
      let encoded = encodeCsv (V.empty :: V.Vector Person)
      encoded `shouldSatisfy` isLeft
