-- file: Adt.hs
-- author: Jacob Xie
-- date: 2025/09/18 13:27:31 Thursday
-- brief:

-- |
-- Module: HCsv.Adt
-- Description: Shared aliases for rows, fields, and headers.
--
-- These aliases keep the surface API succinct and make type signatures
-- easier to scan throughout the rest of the package.

module HCsv.Adt
  ( module HCsv.Adt,
  )
where

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.Vector

-- | CSV data represented as a Haskell vector of vector of
-- bytestrings.
type Csv = Vector Record

-- | A record corresponds to a single line in a CSV file.
type Record = Vector Field

-- | The header corresponds to the first line a CSV file. Not all CSV
-- files have a header.
type Header = Vector Name

-- | A header has one or more names, describing the data in the column
-- following the name.
type Name = BS.ByteString

-- | A record corresponds to a single line in a CSV file, indexed by
-- the column name rather than the column index.
type NamedRecord = HM.HashMap BS.ByteString BS.ByteString

-- | A single field within a record.
type Field = BS.ByteString

-- | Cached mapping from header names to positional indices used while
-- parsing records.
type FieldIndices = HM.HashMap BS.ByteString Int
