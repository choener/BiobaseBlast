
-- TODO some unit tests to check if assumed-known values are in the right
-- table cells

module Main where

import Test.Tasty
import Test.Tasty.Silver.Advanced
import Biobase.BLAST.Import
import System.FilePath ( replaceExtension )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad

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
    goldenFilePath :: FilePath
    goldenFilePath = replaceExtension testFile ".golden"
    in goldenTest1 "Tabular HTTP blast" (liftM Just (TIO.readFile goldenFilePath)) (testBlast testFile) compareText (ShowText) print
    --in goldenVsAction "Tabularblast" goldenFile (blastFromFile testFile) (\a -> T.pack $ concatMap show a)

testBlast :: FilePath -> IO T.Text
testBlast testFile = do
  result <- blastFromFile testFile
  let textresult = T.pack $ ((concatMap show result)++ "\n")
  return textresult

succeedHTTPTest :: IO TestTree
succeedHTTPTest = do
  return $ testGroup "succeed" $ [tastyHTTPTest "tests/succeed/httptabular.test"]

tastyHTTPTest :: FilePath -> TestTree
tastyHTTPTest testFile =
  let
    goldenFilePath :: FilePath
    goldenFilePath = replaceExtension testFile ".golden"
    in goldenTest1 "Tabular HTTP blast" (liftM Just (TIO.readFile goldenFilePath)) (testBlastHTTPS testFile) compareText (ShowText) print
  --in goldenVsAction "Tabular HTTP blast" goldenFile (blastHTTPFromFile testFile) (\a -> T.pack $ concatMap show a)

testBlastHTTPS :: FilePath -> IO T.Text
testBlastHTTPS testFile = do
  result <- blastHTTPFromFile testFile
  let textresult = T.pack $ ((concatMap show result) ++ "\n")
  return textresult

compareText :: T.Text -> T.Text -> GDiff
compareText goldT testT
  | goldT == testT = Equal
  | otherwise = DiffText (Just ("Golden file and test output are different:\nTest:\n" ++ T.unpack testT ++ "\nGold:\n" ++ T.unpack goldT)) testT goldT
