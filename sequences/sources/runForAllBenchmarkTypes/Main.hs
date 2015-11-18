module Main where


import qualified Data.Char as DC (
    isSpace
    )

import qualified Data.List as DL (
      group
    , length
    , sort
    )

import qualified System.Directory as SD (
    copyFile
    )

import qualified System.Process as SP (
      CreateProcess ( .. )
    , StdStream ( .. )
    , callCommand
    , callProcess
    , createProcess
    , proc
    , readProcess
    , shell
    , showCommandForUser
    )

import qualified Data.List as DL (
      intersperse
    , isPrefixOf
    , stripPrefix
    )

import qualified Data.Text.Lazy as DTL (
      Text ( .. )
    , all
    , append
    , breakOn
    , dropWhile
    , concat
    --, intersperse
    , isPrefixOf
    , lines
    , null
    , pack
    , replace
    , split
    , splitOn
    , strip
    , stripStart
    , takeWhile
    , unpack
    )

import qualified Data.Text.Lazy.IO as DTLI (
      hGetContents
    , readFile
    , writeFile
    )

{-import qualified Text.CSV as TC (
    printCSV )
-}


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

import Paths

import CommonFunctions

import Types

import UnitConv


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

main :: IO ()
main = do
    --patchFileMainForRunForAllForTime ( runForAllSequenceImplementationsSourceFilesPath ++ "Main.hs.template" )
    
    SP.callCommand "cabal configure"
    
    SP.callCommand "cabal build runForAllSequenceImplementations"

    ( _ , Just hout , _ , _ ) <- SP.createProcess ( ( SP.proc "./dist/build/runForAllSequenceImplementations/runForAllSequenceImplementations" [] ){ SP.std_out = SP.CreatePipe } )
    timeBenchmarkOutput <- DTLI.hGetContents hout
    DTLI.writeFile "delete-output.csv" timeBenchmarkOutput

    --print timeBenchmarkOutput

    {-
    SP.callCommand "cabal clean"
    

    patchFileMainForRunForAllForEnergy ( runForAllSequenceImplementationsSourceFilesPath ++ "Main.hs.template" )
    
    SP.callCommand "cabal configure"
    
    SP.callCommand "cabal build runForAllSequenceImplementations"

    ( _ , Just hout , _ , _ ) <- SP.createProcess ( ( SP.proc "./dist/build/runForAllSequenceImplementations/runForAllSequenceImplementations" [] ){ SP.std_out = SP.CreatePipe } )
    energyBenchmarkOutput <- DTLI.hGetContents hout

    let
        io1 = DTL.replace ( DTL.pack "\"s\"" ) ( DTL.pack "\"j\"") energyBenchmarkOutput
        io2 = DTL.replace ( DTL.pack "\"ms\"" ) ( DTL.pack "\"mj\"") io1
        io3 = DTL.replace ( DTL.pack "\"\\956s\"" ) ( DTL.pack "\"\\956j\"") io2
        io4 = DTL.replace ( DTL.pack "\"ns\"" ) ( DTL.pack "\"nj\"") io3
        io5 = DTL.replace ( DTL.pack "\"ps\"" ) ( DTL.pack "\"pj\"") io4

    DTLI.writeFile "energy.csv" io5 --energyBenchmarkOutput

    --print energyBenchmarkOutput

    SP.callCommand "cabal clean"
    -}

-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

{-

patchFileMainForRunForAllForEnergy :: FilePath -> IO ()
patchFileMainForRunForAllForEnergy f = do
    fileContents <- DTLI.readFile f
    let
        io1 = DTL.replace ( DTL.pack "<csvHeader>" ) ( DTL.pack "\",EnergyConsumption,Units\"") fileContents
        io2 = DTL.replace ( DTL.pack "<benchmarkIdPrefix>" ) ( DTL.pack "\"EnergyConsumption/Sequence\"") io1 
        io3 = DTL.replace ( DTL.pack "<bmType>" ) ( DTL.pack "energyConsumption") io2 
        output = io3

        outputFile = reverse . maybe [] id . DL.stripPrefix ( reverse ".template" ) . reverse $ f
    DTLI.writeFile outputFile output

-}

-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

{-

patchFileMainForRunForAllForTime :: FilePath -> IO ()
patchFileMainForRunForAllForTime f = do
    fileContents <- DTLI.readFile f
    let
        io1 = DTL.replace ( DTL.pack "<csvHeader>" ) ( DTL.pack "\",Time,Units\"") fileContents
        io2 = DTL.replace ( DTL.pack "<benchmarkIdPrefix>" ) ( DTL.pack "\"Time/Sequence\"") io1 
        io3 = DTL.replace ( DTL.pack "<bmType>" ) ( DTL.pack "time") io2 
        output = io3

        outputFile = reverse . maybe [] id . DL.stripPrefix ( reverse ".template" ) . reverse $ f
    DTLI.writeFile outputFile output

-}

-- ---------- ---------- ---------- ---------- ---------- ---------- ----------


