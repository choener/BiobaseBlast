
-- TODO some unit tests to check if assumed-known values are in the right
-- table cells

module Main where

import Test.Tasty
import Test.Tasty.Silver
import Biobase.BLAST.Import
import System.FilePath ( replaceExtension )
import qualified Data.Text as T

main :: IO ()
main = defaultMain =<< allTests

allTests :: IO TestTree
allTests = do
  tabularSucceedTest <- succeedTest
  tabularHTTPSucceedTest <- succeedHTTPTest
  return $ testGroup "tests"
    [tabularSucceedTest, tabularHTTPSucceedTest]

succeedTest :: IO TestTree
succeedTest = do
  return $ testGroup "succeed" $ [tastyTest "tests/succeed/tabular.test"]

tastyTest :: FilePath -> TestTree
tastyTest testFile =
  let
    goldenFile :: FilePath
    goldenFile = replaceExtension testFile ".golden"
    in goldenVsAction "Tabularblast" goldenFile (blastFromFile testFile) (\a -> T.pack $ concatMap show a)

succeedHTTPTest :: IO TestTree
succeedHTTPTest = do
  return $ testGroup "succeed" $ [tastyHTTPTest "tests/succeed/httptabular.test"]

tastyHTTPTest :: FilePath -> TestTree
tastyHTTPTest testFile =
  let
    goldenFile :: FilePath
    goldenFile = replaceExtension testFile ".golden"
    in goldenVsAction "Tabular HTTP blast" goldenFile (blastHTTPFromFile testFile) (\a -> T.pack $ concatMap show a)
