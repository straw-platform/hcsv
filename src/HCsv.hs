{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- file: HCsv.hs
-- author: Jacob Xie
-- date: 2025/09/18 14:00:03 Thursday
-- brief:

module HCsv
  ( FromRecord (..),
    Parser (..),
    readCsvRaw,
    readCsv,
    (~>),
    (~>?),
    module HCsv.Adt,
  )
where


import qualified Data.Attoparsec.Lazy as AL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import GHC.Float (double2Float)
import HCsv.Adt
import HCsv.Parser
import HCsv.Util
import System.IO


----------------------------------------------------------------------------------------------------
-- Class
----------------------------------------------------------------------------------------------------

-- Typeclass for parsing a single record
class FromRecord a where
  parseRecord :: FieldIndices -> Record -> Either String a

class Parser a where
  parse :: BS.ByteString -> a

----------------------------------------------------------------------------------------------------
-- Private Fn
----------------------------------------------------------------------------------------------------

-- Get indices for fields from the first record
getFieldIndices :: Record -> HM.HashMap BS.ByteString Int
getFieldIndices = V.ifoldl' (\acc i bs -> HM.insert bs i acc) HM.empty

instance Parser String where
  parse = T.unpack . TE.decodeUtf8Lenient

instance Parser (Maybe String) where
  parse s = case f s of
    "" -> Nothing
    s' -> Just s'
    where
      f = T.unpack . TE.decodeUtf8Lenient

instance Parser Int where
  parse s = case TR.decimal . TE.decodeUtf8Lenient $ s of
    Right (i, r) | r == T.empty -> i
    _ -> 0

instance Parser (Maybe Int) where
  parse s = case TR.decimal . TE.decodeUtf8Lenient $ s of
    Right (i, r) | r == T.empty -> Just i
    _ -> Nothing

instance Parser Float where
  parse s = case TR.double . TE.decodeUtf8Lenient $ s of
    Right (i, r) | r == T.empty -> double2Float i
    _ -> 0

instance Parser (Maybe Float) where
  parse s = case TR.double . TE.decodeUtf8Lenient $ s of
    Right (i, r) | r == T.empty -> Just $ double2Float i
    _ -> Nothing

instance Parser Double where
  parse s = case TR.double . TE.decodeUtf8Lenient $ s of
    Right (i, r) | r == T.empty -> i
    _ -> 0

instance Parser (Maybe Double) where
  parse s = case TR.double . TE.decodeUtf8Lenient $ s of
    Right (i, r) | r == T.empty -> Just i
    _ -> Nothing

instance Parser Bool where
  parse s = case T.toLower $ f s of
    "true" -> True
    _ -> False
    where
      f = TE.decodeUtf8Lenient

instance Parser (Maybe Bool) where
  parse s = case T.toLower $ f s of
    "true" -> Just True
    "false" -> Just False
    _ -> Nothing
    where
      f = TE.decodeUtf8Lenient

----------------------------------------------------------------------------------------------------
-- Public Fn
----------------------------------------------------------------------------------------------------

(~>) :: (Parser a) => (HM.HashMap BS.ByteString Int, BS.ByteString) -> Record -> Either String a
(~>) (fieldIndices, fld) vec = do
  idx <- maybe2Either "field not found" $ HM.lookup fld fieldIndices
  return $ parse $ vec V.! idx

(~>?) :: (Parser (Maybe a)) => (HM.HashMap BS.ByteString Int, BS.ByteString) -> Record -> Either String (Maybe a)
(fieldIndices, fld) ~>? vec =
  case HM.lookup fld fieldIndices of
    Nothing -> Right Nothing
    Just idx -> Right (parse (vec V.! idx))

-- Define a function to parse a CSV file into a vector of vectors of ByteString
readCsvRaw :: FilePath -> IO (Either String Csv)
readCsvRaw f = do
  h <- openFile f ReadMode
  b <- BS.hGetContents h

  return $ AL.parseOnly csv (BSL.fromStrict b)

readCsv :: (FromRecord a) => FilePath -> IO (Either String (V.Vector a))
readCsv f = do
  raw <- readCsvRaw f

  case raw of
    Left err -> return $ Left err
    Right r -> do
      let hd = V.head r
          fi = getFieldIndices hd
          records = V.tail r
          res = traverse (parseRecord fi) records

      return res
