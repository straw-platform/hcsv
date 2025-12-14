{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- file: HCsv.hs
-- author: Jacob Xie
-- date: 2025/09/18 14:00:03 Thursday
-- brief:

module HCsv
  ( FromRecord (..),
    ToField (..),
    ToRecord (..),
    Parser (..),
    readCsvRaw,
    readCsv,
    encodeCsv,
    writeCsv,
    (~>),
    (~>?),
    module HCsv.Adt,
  )
where


import qualified Data.Attoparsec.Lazy as AL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Builder (byteString, charUtf8, toLazyByteString)
import qualified Data.HashMap.Strict as HM
import Data.List (intersperse)
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

class ToField a where
  toField :: a -> BS.ByteString

class (FromRecord a) => ToRecord a where
  toNamedFields :: a -> [(BS.ByteString, BS.ByteString)]

----------------------------------------------------------------------------------------------------
-- Private Fn
----------------------------------------------------------------------------------------------------

-- Get indices for fields from the first record
getFieldIndices :: Record -> HM.HashMap BS.ByteString Int
getFieldIndices = V.ifoldl' (\acc i bs -> HM.insert bs i acc) HM.empty

instance ToField BS.ByteString where
  toField = id

instance ToField String where
  toField = TE.encodeUtf8 . T.pack

instance (ToField a) => ToField (Maybe a) where
  toField = maybe "" toField

instance ToField Int where
  toField = BSC.pack . show

instance ToField Float where
  toField = BSC.pack . show

instance ToField Double where
  toField = BSC.pack . show

instance ToField Bool where
  toField True = "true"
  toField False = "false"

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

toHeaderAndRecord :: [(BS.ByteString, BS.ByteString)] -> (Header, Record)
toHeaderAndRecord pairs =
  (V.fromList $ fmap fst pairs, V.fromList $ fmap snd pairs)

ensureRecordMatchesHeader :: Header -> [(BS.ByteString, BS.ByteString)] -> Either String Record
ensureRecordMatchesHeader hdr pairs =
  let names = fmap fst pairs
   in if names == V.toList hdr
        then Right (V.fromList $ fmap snd pairs)
        else Left "record fields do not match header"

escapeField :: BS.ByteString -> BS.ByteString
escapeField bs
  | needsEscape = BS.concat ["\"", BS.concatMap escape bs, "\""]
  | otherwise = bs
  where
    needsEscape = BS.any (\c -> c == doubleQuote || c == decDelimiter || c == newline || c == cr) bs
    escape c
      | c == doubleQuote = "\"\""
      | otherwise = BS.singleton c

renderRecord :: V.Vector BS.ByteString -> BSL.ByteString
renderRecord =
  toLazyByteString
    . mconcat
    . intersperse (charUtf8 $ toEnum $ fromIntegral decDelimiter)
    . fmap (byteString . escapeField)
    . V.toList

renderCsv :: Header -> V.Vector Record -> BSL.ByteString
renderCsv hdr records =
  let csvLines = renderRecord hdr : V.toList (renderRecord <$> records)
   in BSL.intercalate (BSL.singleton newline) csvLines

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

encodeCsv :: (ToRecord a) => V.Vector a -> Either String BSL.ByteString
encodeCsv values
  | V.null values = Left "empty CSV data"
  | otherwise = do
      let firstPairs = toNamedFields (V.head values)
          (hdr, firstRecord) = toHeaderAndRecord firstPairs
      restRecords <- traverse (ensureRecordMatchesHeader hdr . toNamedFields) (V.tail values)
      let records = firstRecord `V.cons` restRecords
      return $ renderCsv hdr records

writeCsv :: (ToRecord a) => FilePath -> V.Vector a -> IO (Either String ())
writeCsv f values =
  case encodeCsv values of
    Left err -> return $ Left err
    Right bytes -> do
      BSL.writeFile f bytes
      return $ Right ()
