{-{-# LANGUAGE ScopedTypeVariables #-}-}


module Main where


import qualified Data.Char as DC (
    isSpace
    )

import qualified Data.List as DL (
      group
    , length
    , sort
    )

import Data.Time

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
    , fromChunks
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

import qualified Data.Text.IO as DTI (
      hGetContents
    , readFile
    , writeFile
    )

-- ---------- ---------- ---------- ---------- ---------- ---------- ----------
import System.IO
-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

import Paths

import CommonFunctions

import Types

import UnitConv


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

main :: IO ()
main = do

    assocCollectionImplementations <- getAssocCollectionImplementationsFromFile assocCollectionImplementationsFilePath
        :: IO [ AssocCollectionId ]

    benchmarksGroupedByAssocCollectionImplementation <- mapM getBenchmarksForAssocCollectionImplementation assocCollectionImplementations
        :: IO [ [ BenchmarkId ] ]

    let
        benchmarksGroupedByBenchmark = groupHeads benchmarksGroupedByAssocCollectionImplementation
            :: [ [ BenchmarkId ] ]

    benchmarksOutputsForAllAssocCollectionImplementations <- mapM ( flip getBenchmarkOutputForAllAssocCollectionImplementations assocCollectionImplementations ) benchmarksGroupedByBenchmark
        :: IO [ [ BenchmarkOutput ] ]
    --print benchmarksOutputsForAllAssocCollectionImplementations


    -- Times
    {-
    let
        executionTimes = map ( map getTime ) benchmarksOutputsForAllAssocCollectionImplementations
            :: [ [ ( DTL.Text , Double , DTL.Text ) ] ]
    --let
        mostObservedUnitsInTimes = map ( snd . head . reverse . DL.sort . rle . map ( \ ( _ , _ , u ) -> u ) ) executionTimes
            :: [ DTL.Text ]
    --print mostObservedUnitsInTimes

    let
        finalValuesForTimes = convertUnitsToMostObservedUnit executionTimes mostObservedUnitsInTimes
            :: [ [ ( DTL.Text , Double , DTL.Text ) ] ]

    --let
        timesOutput = map ( map (\ (x , y ,z) -> [ x , DTL.pack ( show y ) , z] ) ) finalValuesForTimes
            :: [ [ [ DTL.Text ] ] ]
    --print output

    --let
        timesOutputCSV = DTL.pack . concat . (:) ",Time,Units\n" . DL.intersperse "\n" . map ( concat . DL.intersperse "\n" . map ( concat . DL.intersperse "," . map show ) ) $ timesOutput
            :: DTL.Text
    -}

    timesOutputCSV <- processFeature "time" benchmarksOutputsForAllAssocCollectionImplementations
    DTLI.writeFile "time-Time.csv" timesOutputCSV :: IO ()


    -- Energy
    {-
    let
        energyConsumptions = map ( map getEnergy ) benchmarksOutputsForAllAssocCollectionImplementations 
            :: [ [ ( DTL.Text , Double , DTL.Text ) ] ]
    --print energyConsumptions

    --let
        energyOutput = map ( map (\ (x , y ,z) -> [ x , DTL.pack ( show y ) , z] ) ) energyConsumptions
            :: [ [ [ DTL.Text ] ] ]

    --let
        energyOutputCSV = DTL.pack . concat . (:) ",Energy,Units\n" . DL.intersperse "\n" . map ( concat . DL.intersperse "\n" . map ( concat . DL.intersperse "," . map show ) ) $ energyOutput
            :: DTL.Text
    -}
    energyOutputCSV <- processFeature "energy" benchmarksOutputsForAllAssocCollectionImplementations
    DTLI.writeFile "energy-Energy.csv" energyOutputCSV :: IO ()


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

processFeature :: String -> [ [ BenchmarkOutput ] ] -> IO ( DTL.Text )
processFeature feature benchmarksOutputsForAllAssocCollectionImplementations = do
    let
        values = case feature of
            "time" -> map ( map getTime ) benchmarksOutputsForAllAssocCollectionImplementations
            "energy" -> map ( map getEnergy ) benchmarksOutputsForAllAssocCollectionImplementations
                :: [ [ ( DTL.Text , Double , DTL.Text ) ] ]
    --print values
    
    let
        output = map ( map (\ (x , y ,z) -> [ x , DTL.pack ( show y ) , z] ) ) values
            :: [ [ [ DTL.Text ] ] ]

        outputCSV = DTL.pack . concat
            . (:) ( case feature of
                "time" -> ",Time,Units\n"
                "energy" -> ",Energy,Units\n"
                )
            . DL.intersperse "\n" . map ( concat . DL.intersperse "\n" . map ( concat . DL.intersperse "," . map show ) ) $ output
            :: DTL.Text
    return outputCSV



-- ---------- ---------- ---------- ---------- ---------- ---------- ----------
{-
--uni = [ "s", "s", "s","s","s","s","s","s","s","s"]

convertUnitsToMostObservedUnit values units =
    let
        fus = zip values units
    in
        map (uncurry convUnits') fus


convUnits' l u = map f l
    where
        f ( si , v , iu ) = ( si , cvu v iu u , u )

cvu v iuStr ouStr = convert v ( uStrToUnit ( DTL.unpack iuStr ) ) ( uStrToUnit ( DTL.unpack ouStr ) ) 


uStrToUnit "s" = Unit
uStrToUnit "ms" = Milli
--uStrToUnit "µs" = Micro
uStrToUnit "\956s" = Micro
uStrToUnit "ns" = Nano
uStrToUnit "ps" = Pico
-}

-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

{-
    |
        The 'getTime' function extracts the benchmark name, execution time and this time's units from the benchmark output.
        It's parameters are:
            a benchmark output
        It's output is:
            a triple ( benchmark identifier , time , time units )

-}

getTime :: BenchmarkOutput -> ( DTL.Text , Double , DTL.Text )
getTime bmOutput =
    let
        ls = take 2 . DTL.lines $ bmOutput
        bmName = DTL.append ( DTL.pack "Time/" ) . snd . DTL.breakOn ( DTL.pack "AssocCollection/" ) . head . take 1 $ ls
        tl = filter ( not . DTL.null ) . DTL.split ( DC.isSpace ) . head . drop 1 $ ls

        -- Here, 0 should probably be 1!
        snd_tl = if length tl > 0 then ( DTL.unpack ( tl !! 1 ) ) else error "tl, in getTime should have at least 2 elements: " ++ ( show tl )
        third_tl = if length tl > 2 then ( DTL.unpack ( tl !! 2 ) ) else error "tl, in getTime should have at least 3 elements: " ++ show tl
    in
        ( bmName , read ( snd_tl ) :: Double , DTL.pack third_tl )


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

{-
    |
        The 'getEnergy' function extracts the benchmark name, energy consumption and this value's units from the benchmark output.
        It's parameters are:
            a benchmark output
        It's output is:
            a triple ( benchmark identifier , energy , energy units )

-}

getEnergy :: BenchmarkOutput -> ( DTL.Text , Double , DTL.Text )
getEnergy bmOutput =
    let
        ls = take 7 . DTL.lines $ bmOutput
        bmName = DTL.append ( DTL.pack "EnergyConsumption/" ) . snd . DTL.breakOn ( DTL.pack "AssocCollection/" ) . head . take 1 $ ls
        tl = filter ( not . DTL.null ) . DTL.split ( DC.isSpace ) . head . drop 6 $ ls

        -- Here, 0 should probably be 1!
        snd_tl = if length tl > 0 then ( DTL.unpack ( tl !! 1 ) ) else error "tl, in getEnergy should have at least 2 elements: " ++ ( show tl )

    in
        ( bmName , read ( snd_tl ) :: Double , DTL.pack "j" ) -- NOTE: Use "secs" from Criterion?


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

{-
    |
        The function 'getBenchmarksForAssocCollectionImplementation' runs an (Criterion generated) executable for (the benchmarks of) a associative collection implementation, to list the benchmarks included in that executable.
        It's parameters are:
            a associative collection implementation identifier
        It's output is:
            a list of benchmark identifiers included in the executable

-}

getBenchmarksForAssocCollectionImplementation :: AssocCollectionId -> IO [ BenchmarkId ]
getBenchmarksForAssocCollectionImplementation s = do
    let 
        benchmarkCommand = benchmarksExecutablesDestinationPath ++ benchmarkExecutableBaseName ++ DTL.unpack s
    ( _ , Just hout , _ , _ ) <- SP.createProcess ( ( SP.proc benchmarkCommand [ "--list" ] ){ SP.std_out = SP.CreatePipe } )
    benchmarkCommandOutput <- DTLI.hGetContents hout

    return . DTL.lines $ benchmarkCommandOutput


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

{-
    |
        The 'getBenchmarkOutputForAllAssocCollectionImplementations' runs a specific benchmark for a list of associative collection implementations.
        It's parameters are:
            a list of benchmark identifiers (all being the same benchmark, but for different associative collection implementations)
            a list of associative collection implementations
        It's output is:
            a list of the output of the benchmarks

-}

getBenchmarkOutputForAllAssocCollectionImplementations :: [ BenchmarkId ] -> [ AssocCollectionId ] -> IO [ BenchmarkOutput ]
getBenchmarkOutputForAllAssocCollectionImplementations bl sil =
    mapM (uncurry getBenchmarkOutputForAssocCollectionImplementation ) $ zip bl sil


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

{-
    |

        The 'getBenchmarkOutputForAssocCollectionImplementation' function, runs a specific benchmark for one associative collection implementation.
        It's parameters are:
            a benchmark identifier;
            an associative collection implementation identifier.
        It's output is:
            the textual output from the benchmark.

-}

getBenchmarkOutputForAssocCollectionImplementation :: BenchmarkId -> AssocCollectionId -> IO BenchmarkOutput
getBenchmarkOutputForAssocCollectionImplementation b s = do
    let
        benchmarkCommand = benchmarksExecutablesDestinationPath ++ benchmarkExecutableBaseName ++ DTL.unpack s
        benchmarkId = "" ++ DTL.unpack b ++ "" -- Note: nothing around!

    hPutStrLn stderr $ SP.showCommandForUser benchmarkCommand [ "--regress", "packageEnergy:iters" , benchmarkId ]

    it <- getCurrentTime
    hPutStrLn stderr . (++) "Started: " . show $ it

    ( _ , Just hout , _ , _ ) <- SP.createProcess ( ( SP.proc benchmarkCommand [ "--regress",  "packageEnergy:iters" , benchmarkId ] ){ SP.std_out = SP.CreatePipe } )
    benchmarkOutput <- DTI.hGetContents hout

    hPutStrLn stderr . DTL.unpack $ DTL.fromChunks [ benchmarkOutput ]

    ft <- getCurrentTime
    hPutStrLn stderr . (++) "Finished: " . show $ ft

    return ( DTL.fromChunks [ benchmarkOutput ])


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------


