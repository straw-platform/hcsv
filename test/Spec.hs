-- file: Spec.hs
-- author: Jacob Xie
-- date: 2025/09/18 13:59:45 Thursday
-- brief:


module Main where

import Test.Hspec
import qualified HCsvSpec

main :: IO ()
main = hspec HCsvSpec.spec
