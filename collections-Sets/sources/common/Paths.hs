module Paths where


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

setImplementationsFilePath :: String
setImplementationsFilePath = "setImplementations.txt"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

benchmarkSourceFilesPath = "sources/benchmarkForOneSetImpl/"

sourceFilesToPatchForBenchmark = map ( (++) benchmarkSourceFilesPath) [ "Main.hs.template" ]


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

benchmarkExecutableFilePath :: String
benchmarkExecutableFilePath = "dist/build/benchmarkForOneSetImpl/benchmarkForOneSetImpl"

benchmarkExecutableBaseName :: String
benchmarkExecutableBaseName = "benchmarkForOneSetImpl-"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

commonSourceFilesPath :: FilePath
commonSourceFilesPath = "sources/common/"

commonSourceFilesToPatch :: [ FilePath ]
commonSourceFilesToPatch = map ( (++) commonSourceFilesPath ) [ "Environments.hs.template", "Ops.hs.template" ]


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

benchmarksExecutablesDestinationPath :: FilePath
benchmarksExecutablesDestinationPath = "tmp/executables/"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

runForAllSetImplementationsSourceFilesPath :: FilePath
runForAllSetImplementationsSourceFilesPath = "sources/runForAllSetImplementations/"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

csvFileSetsFilesPath :: FilePath
csvFileSetsFilesPath = "csvFileSets/"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------


