dyn.load(paste("ENTER_YOUR_ADOLC_SOURCE_FOLDER_HERE/ADOL-C/swig/R/adolc", .Platform$dynlib.ext, sep=""))
source("src/adolc_dispatch.R")
source("src/adolc.R")
source("src/adolc_summary.R")
source("src/adolc_constructor.R")
cacheMetaData(1)

