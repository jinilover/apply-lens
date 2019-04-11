{-# LANGUAGE DisambiguateRecordFields #-}
module Main where

import Protolude 
import Prelude (String)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as B

import Parliament.Common
import Parliament.DataTypes
import Parliament.StructureBuildup
import Parliament.FundDistribution
import Parliament.Utils

main :: IO ()
main = do
    (inFile : outFile : _) <- getArgs
    dataObj <- readJson inFile
    proc outFile dataObj

proc :: String -> Either String Gazette -> IO ()
proc _ (Left err) = putStrLn err
proc outFile (Right g@Gazette{..}) = 
    let (fbs, dfs) = (chargeDistricts . approveFund . groupFunds . capByAvailableFund) g _districts
        report = Report fbs dfs in
    B.writeFile outFile (encodePretty report) >> 
    putStrLn (outFile ++ " generated")
