module Paths where


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

sequenceImplementationsFilePath :: String
sequenceImplementationsFilePath = "sequenceImplementations.txt"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

benchmarkSourceFilesPath = "sources/benchmarkForOneSeqImpl/"

sourceFilesToPatchForBenchmark = map ( (++) benchmarkSourceFilesPath) [ "Main.hs.template" ]


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

benchmarkExecutableFilePath :: String
benchmarkExecutableFilePath = "dist/build/benchmarkForOneSeqImpl/benchmarkForOneSeqImpl"

benchmarkExecutableBaseName :: String
benchmarkExecutableBaseName = "benchmarkForOneSeqImpl-"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

commonSourceFilesPath :: FilePath
commonSourceFilesPath = "sources/common/"

commonSourceFilesToPatch :: [ FilePath ]
commonSourceFilesToPatch = map ( (++) commonSourceFilesPath ) [ "Environments.hs.template", "Ops.hs.template" ]


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

benchmarksExecutablesDestinationPath :: FilePath
benchmarksExecutablesDestinationPath = "tmp/executables/"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

runForAllSequenceImplementationsSourceFilesPath :: FilePath
runForAllSequenceImplementationsSourceFilesPath ="sources/runForAllSequenceImplementations/"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

csvFileSetsFilesPath :: FilePath
csvFileSetsFilesPath = "csvFileSets/"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------


