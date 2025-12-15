# HCsv

HCsv is a tiny CSV toolkit focused on \"open the file, get vectors of
records\" workflows.  It layers a handful of ergonomic typeclasses on
top of Attoparsec so you can keep using plain `ByteString`/`Vector`
while still writing friendly domain code.

## Features

- Header aware parsing with `FromRecord` instances that can look up
  fields by name.
- Streaming-safe parser implemented with Attoparsec (handles big files
  without loading the whole thing at once).
- Round-trip encoding helpers (`ToRecord`, `encodeCsv`, `writeCsv`) so
  you can persist modified data with correct escaping.
- Small utility layer with a readable operator DSL (`(~>)`, `(~>?)`)
  for pulling typed values out of rows.

## Quickstart

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Vector as V
import HCsv

data Person = Person { name :: String, age :: Int } deriving (Show)

instance FromRecord Person where
  parseRecord fi rec = Person
    <$> (fi, "name") ~> rec
    <*> (fi, "age")  ~> rec

people <- readCsv "people.csv"
case people of
  Right vec -> V.forM_ vec print
  Left err  -> putStrLn ("Could not parse CSV: " <> err)
```

To encode rows back into CSV:

```haskell
instance ToRecord Person where
  toNamedFields p =
    [ ("name", toField (name p))
    , ("age", toField (age p))
    ]

_ <- writeCsv "output.csv" (V.fromList [Person "Ada" 28])
```

## Development

- `cabal update && cabal build` — compile the library with warnings enabled.
- `cabal repl hcsv` — explore the modules in GHCI.
- `cabal test hcsv-test` — run the Hspec suite.  Tests rely on
  temporary files; no fixtures need to be committed.

All modules use two-space indentation and keep helpers in
`HCsv.Util`.  See `test/HCsvSpec.hs` for examples that cover empty
files, invalid fields, and escaping edge cases.
