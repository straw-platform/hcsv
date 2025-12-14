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


----------------------------------------------------------------------------------------------------
-- Class
----------------------------------------------------------------------------------------------------

-- Typeclass for parsing a single record
class FromRecord a where
  parseRecord :: FieldIndices -> Record -> Either String a

class Parser a where
  parse :: BS.ByteString -> Either String a

----------------------------------------------------------------------------------------------------
-- Private Fn
----------------------------------------------------------------------------------------------------

-- Get indices for fields from the first record
getFieldIndices :: Record -> HM.HashMap BS.ByteString Int
getFieldIndices = V.ifoldl' (\acc i bs -> HM.insert bs i acc) HM.empty

instance Parser String where
  parse = Right . T.unpack . TE.decodeUtf8Lenient

instance Parser (Maybe String) where
  parse s = case f s of
    "" -> Right Nothing
    s' -> Right (Just s')
    where
      f = T.unpack . TE.decodeUtf8Lenient

instance Parser Int where
  parse s = case TR.decimal . TE.decodeUtf8Lenient $ s of
    Right (i, r) | r == T.empty -> Right i
    _ -> Left "invalid Int"

instance Parser (Maybe Int) where
  parse s = case TR.decimal . TE.decodeUtf8Lenient $ s of
    Right (i, r) | r == T.empty -> Right (Just i)
    _ -> Right Nothing

instance Parser Float where
  parse s = case TR.double . TE.decodeUtf8Lenient $ s of
    Right (i, r) | r == T.empty -> Right (double2Float i)
    _ -> Left "invalid Float"

instance Parser (Maybe Float) where
  parse s = case TR.double . TE.decodeUtf8Lenient $ s of
    Right (i, r) | r == T.empty -> Right (Just $ double2Float i)
    _ -> Right Nothing

instance Parser Double where
  parse s = case TR.double . TE.decodeUtf8Lenient $ s of
    Right (i, r) | r == T.empty -> Right i
    _ -> Left "invalid Double"

instance Parser (Maybe Double) where
  parse s = case TR.double . TE.decodeUtf8Lenient $ s of
    Right (i, r) | r == T.empty -> Right (Just i)
    _ -> Right Nothing

instance Parser Bool where
  parse s = case T.toLower $ f s of
    "true" -> Right True
    "false" -> Right False
    other -> Left $ "invalid Bool: " ++ T.unpack other
    where
      f = TE.decodeUtf8Lenient

instance Parser (Maybe Bool) where
  parse s = case T.toLower $ f s of
    "true" -> Right (Just True)
    "false" -> Right (Just False)
    _ -> Right Nothing
    where
      f = TE.decodeUtf8Lenient

----------------------------------------------------------------------------------------------------
-- Public Fn
----------------------------------------------------------------------------------------------------

(~>) :: (Parser a) => (HM.HashMap BS.ByteString Int, BS.ByteString) -> Record -> Either String a
(~>) (fieldIndices, fld) vec = do
  idx <- maybe2Either "field not found" $ HM.lookup fld fieldIndices
  parse $ vec V.! idx

(~>?) :: (Parser (Maybe a)) => (HM.HashMap BS.ByteString Int, BS.ByteString) -> Record -> Either String (Maybe a)
(fieldIndices, fld) ~>? vec =
  case HM.lookup fld fieldIndices of
    Nothing -> Right Nothing
    Just idx -> parse (vec V.! idx)

-- Define a function to parse a CSV file into a vector of vectors of ByteString
readCsvRaw :: FilePath -> IO (Either String Csv)
readCsvRaw f = do
  b <- BS.readFile f
  return $ AL.parseOnly csv (BSL.fromStrict b)

readCsv :: (FromRecord a) => FilePath -> IO (Either String (V.Vector a))
readCsv f = do
  raw <- readCsvRaw f

  case raw of
    Left err -> return $ Left err
    Right r -> do
      if V.null r
        then return $ Left "empty CSV"
        else do
          let hd = V.head r
              fi = getFieldIndices hd
              records = V.tail r
              res = traverse (parseRecord fi) records
          return res
