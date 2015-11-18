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
    , isSuffixOf
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
    setImplementations <- getSetImplementationsFromFile setImplementationsFilePath
    mapM_ ( \ s -> sequence_ [
            patchAllFilesForSetImplementation s ( sourceFilesToPatchForBenchmark ++ commonSourceFilesToPatch )

            , SP.callCommand "cabal configure"

            , SP.callCommand "cabal build benchmarkForOneSetImpl"
            , SD.copyFile benchmarkExecutableFilePath ( benchmarksExecutablesDestinationPath ++ benchmarkExecutableBaseName ++ DTL.unpack s )

            , SP.callCommand "cabal clean"
            ]
        ) setImplementations

-- Atenção: Sem o "cabal clean" existe um erro aleatório (não compila um, ou seja produz 2 executáveis iguais)
-- Junto com o Professor chegámos à conclusão que o problema tem a ver com a "resolução do relógio" usado pela cabal


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

patchFileForSetImplementation :: DTL.Text -> FilePath -> IO ()
patchFileForSetImplementation s f = do
    fileContents <- DTLI.readFile f
    let
        iOutput = DTL.replace ( DTL.pack "<SetImplementation>" ) s fileContents

        -- Ugly Hack!
        searchStr = "instance NFData a => NFData ( S.Set a )"
        output = if ( ( s == DTL.pack "Data.Edison.Coll.StandardSet" ) && ( DL.isSuffixOf "Ops.hs.template" f ) ) then DTL.replace ( DTL.pack searchStr ) ( DTL.pack ( "--" ++ searchStr ) ) iOutput else iOutput


        outputFile = reverse . maybe [] id . DL.stripPrefix ( reverse ".template" ) . reverse $ f
    DTLI.writeFile outputFile output


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

patchAllFilesForSetImplementation :: DTL.Text -> [ FilePath ] -> IO ()
patchAllFilesForSetImplementation s l = mapM_ ( patchFileForSetImplementation s ) l


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------


