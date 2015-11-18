module Main where


import qualified Data.Char as DC (
    isSpace
    )

import qualified System.Directory as SD (
    copyFile
    )

import qualified System.Process as SP (
    callCommand
    )

import qualified Data.List as DL (
      isPrefixOf
    , stripPrefix
    )
{-
import qualified Data.Text as DT (
      Text ( .. )
    , pack
    , replace
    , unpack
    )

import qualified Data.Text.IO as DTI (
      readFile
    , writeFile
    )
-}
import qualified Data.Text.Lazy as DTL (
      Text ( .. )
    , all
    , dropWhile
    , isPrefixOf
    , lines
    , null
    , pack
    , replace
    , strip
    , stripStart
    , unpack
    )

import qualified Data.Text.Lazy.IO as DTLI (
      readFile
    , writeFile
    )


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

--import Defs

import CommonFunctions

import Paths



-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

main :: IO ()
main = do
    sequenceImplementations <- getSequenceImplementationsFromFile sequenceImplementationsFilePath
    mapM_ ( \ s -> sequence_ [
            patchAllFilesForSequenceImplementation s ( sourceFilesToPatchForBenchmark ++ commonSourceFilesToPatch )

            , SP.callCommand "cabal configure"

            , SP.callCommand "cabal build benchmarkForOneSeqImpl"
            , SD.copyFile benchmarkExecutableFilePath ( benchmarksExecutablesDestinationPath ++ benchmarkExecutableBaseName ++ DTL.unpack s )

            , SP.callCommand "cabal clean"
            ]
        ) sequenceImplementations

-- Atenção: Sem o "cabal clean" existe um erro aleatório (não compila um, ou seja produz 2 executáveis iguais)
-- Junto com o Professor chegámos à conclusão que o problema tem a ver com a "resolução do relógio" usado pela cabal


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

patchFileForSequenceImplementation :: DTL.Text -> FilePath -> IO ()
patchFileForSequenceImplementation s f = do
    fileContents <- DTLI.readFile f
    let
        output = DTL.replace ( DTL.pack "<SequenceImplementation>" ) s fileContents
        outputFile = reverse . maybe [] id . DL.stripPrefix ( reverse ".template" ) . reverse $ f
    DTLI.writeFile outputFile output


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

patchAllFilesForSequenceImplementation :: DTL.Text -> [ FilePath ] -> IO ()
patchAllFilesForSequenceImplementation s l = mapM_ ( patchFileForSequenceImplementation s ) l


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------


