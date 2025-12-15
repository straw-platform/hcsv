{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- file: HCsv.hs
-- author: Jacob Xie
-- date: 2025/09/18 14:00:03 Thursday
-- brief:

-- |
-- Module: HCsv
-- Description: Lightweight CSV reader/writer surface area.
--
-- The 'HCsv' module re-exports the public types, typeclasses, and helpers
-- used across the rest of the package.  High level IO helpers such as
-- 'readCsv' and 'writeCsv' live here together with the small parsing DSL
-- ('Parser', 'FromRecord', 'ToRecord').
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

-- | Typeclass for turning a row into a Haskell value.  The provided
-- 'FieldIndices' makes it possible to look up columns by header name
-- without coupling to the physical column ordering.
class FromRecord a where
  parseRecord :: FieldIndices -> Record -> Either String a

-- | Parses a raw 'BS.ByteString' field into a more structured value.
class Parser a where
  parse :: BS.ByteString -> Either String a

-- | Typeclass for turning typed values back into textual CSV fields.
class ToField a where
  toField :: a -> BS.ByteString

-- | Typeclass for producing a named row that can be encoded into CSV.
class (FromRecord a) => ToRecord a where
  toNamedFields :: a -> [(BS.ByteString, BS.ByteString)]

----------------------------------------------------------------------------------------------------
-- Private Fn
----------------------------------------------------------------------------------------------------

-- | Build a map from header names to their column indices.  This is
-- evaluated once per CSV load and handed down to each 'FromRecord'
-- instance so repeated lookups stay cheap.
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

-- | Ensure the record produced by a 'ToRecord' instance still matches
-- the original header ordering.  This guards against accidentally
-- emitting mismatched shapes when encoding a vector of values.
ensureRecordMatchesHeader :: Header -> [(BS.ByteString, BS.ByteString)] -> Either String Record
ensureRecordMatchesHeader hdr pairs =
  let names = fmap fst pairs
   in if names == V.toList hdr
        then Right (V.fromList $ fmap snd pairs)
        else Left "record fields do not match header"

-- | Escape a single field according to the RFC4180 rules used within
-- this library (double quotes doubled, commas and newlines wrapped).
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

-- | Render a complete CSV document from a header and a collection of
-- rows, separating each rendered line with @\\n@.
renderCsv :: Header -> V.Vector Record -> BSL.ByteString
renderCsv hdr records =
  let csvLines = renderRecord hdr : V.toList (renderRecord <$> records)
   in BSL.intercalate (BSL.singleton newline) csvLines

----------------------------------------------------------------------------------------------------
-- Public Fn
----------------------------------------------------------------------------------------------------

-- | Lookup the named field in a record using cached indices and run
-- the requested 'Parser'.  This operator is typically used within a
-- 'FromRecord' instance, e.g. @(fi, \"age\") ~> record@.
(~>) :: (Parser a) => (HM.HashMap BS.ByteString Int, BS.ByteString) -> Record -> Either String a
(~>) (fieldIndices, fld) vec = do
  idx <- maybe2Either "field not found" $ HM.lookup fld fieldIndices
  parse $ vec V.! idx

-- | Variant of '~>' that treats missing or empty fields as 'Nothing'
-- rather than failing the whole parse.
(~>?) :: (Parser (Maybe a)) => (HM.HashMap BS.ByteString Int, BS.ByteString) -> Record -> Either String (Maybe a)
(fieldIndices, fld) ~>? vec =
  case HM.lookup fld fieldIndices of
    Nothing -> Right Nothing
    Just idx -> parse (vec V.! idx)

-- | Read a CSV file off disk as-is without interpreting headers or
-- converting the fields.  This is handy for debugging or tooling that
-- wants a thin wrapper around Attoparsec's parser.
readCsvRaw :: FilePath -> IO (Either String Csv)
readCsvRaw f = do
  b <- BS.readFile f
  return $ AL.parseOnly csv (BSL.fromStrict b)

-- | Parse a CSV file into a vector of domain values using the
-- supplied 'FromRecord' instance.  Fails if the file is empty or the
-- records cannot be parsed.
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

-- | Encode a vector of records into CSV bytes, making sure each
-- subsequent row produces the same header layout as the first value.
encodeCsv :: (ToRecord a) => V.Vector a -> Either String BSL.ByteString
encodeCsv values
  | V.null values = Left "empty CSV data"
  | otherwise = do
      let firstPairs = toNamedFields (V.head values)
          (hdr, firstRecord) = toHeaderAndRecord firstPairs
      restRecords <- traverse (ensureRecordMatchesHeader hdr . toNamedFields) (V.tail values)
      let records = firstRecord `V.cons` restRecords
      return $ renderCsv hdr records

-- | Write the provided records to disk, returning 'Left' on encoding
-- errors instead of throwing.
writeCsv :: (ToRecord a) => FilePath -> V.Vector a -> IO (Either String ())
writeCsv f values =
  case encodeCsv values of
    Left err -> return $ Left err
    Right bytes -> do
      BSL.writeFile f bytes
      return $ Right ()
