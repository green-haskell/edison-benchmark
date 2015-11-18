module Paths where


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

heapImplementationsFilePath :: String
heapImplementationsFilePath = "heapImplementations.txt"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

benchmarkSourceFilesPath = "sources/benchmarkForOneHeapImpl/"

sourceFilesToPatchForBenchmark = map ( (++) benchmarkSourceFilesPath) [ "Main.hs.template" ]


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

benchmarkExecutableFilePath :: String
benchmarkExecutableFilePath = "dist/build/benchmarkForOneHeapImpl/benchmarkForOneHeapImpl"

benchmarkExecutableBaseName :: String
benchmarkExecutableBaseName = "benchmarkForOneHeapImpl-"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

commonSourceFilesPath :: FilePath
commonSourceFilesPath = "sources/common/"

commonSourceFilesToPatch :: [ FilePath ]
commonSourceFilesToPatch = map ( (++) commonSourceFilesPath ) [ "Environments.hs.template", "Ops.hs.template" ]


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

benchmarksExecutablesDestinationPath :: FilePath
benchmarksExecutablesDestinationPath = "tmp/executables/"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

runForAllHeapImplementationsSourceFilesPath :: FilePath
runForAllHeapImplementationsSourceFilesPath ="sources/runForAllHeapImplementations/"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

csvFileSetsFilesPath :: FilePath
csvFileSetsFilesPath = "csvFileSets/"


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------


