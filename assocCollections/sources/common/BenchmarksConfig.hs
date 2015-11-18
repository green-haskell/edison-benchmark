module BenchmarksConfig where


import Criterion.Main (
    defaultConfig
    )

import Criterion.Types (
    Config ( .. )
    )


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

-- Benchmarks configurations

myBenchmarkConfig = defaultConfig --{ timeLimit = 2.0 } -- seconds


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------


