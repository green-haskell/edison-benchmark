module Paths where


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

assocCollectionImplementationsFilePath :: String
assocCollectionImplementationsFilePath = "assocCollectionImplementations.txt"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

benchmarkSourceFilesPath = "sources/benchmarkForOneAssocCollectionImpl/"

sourceFilesToPatchForBenchmark = map ( (++) benchmarkSourceFilesPath) [ "Main.hs.template" ]


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

benchmarkExecutableFilePath :: String
benchmarkExecutableFilePath = "dist/build/benchmarkForOneAssocCollectionImpl/benchmarkForOneAssocCollectionImpl"

benchmarkExecutableBaseName :: String
benchmarkExecutableBaseName = "benchmarkForOneAssocCollectionImpl-"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

commonSourceFilesPath :: FilePath
commonSourceFilesPath = "sources/common/"

commonSourceFilesToPatch :: [ FilePath ]
commonSourceFilesToPatch = map ( (++) commonSourceFilesPath ) [ "Environments.hs.template", "Ops.hs.template" ]


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

benchmarksExecutablesDestinationPath :: FilePath
benchmarksExecutablesDestinationPath = "tmp/executables/"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

runForAllAssocCollectionImplementationsSourceFilesPath :: FilePath
runForAllAssocCollectionImplementationsSourceFilesPath ="sources/runForAllAssocCollectionImplementations/"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

csvFileSetsFilesPath :: FilePath
csvFileSetsFilesPath = "csvFileSets/"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------


