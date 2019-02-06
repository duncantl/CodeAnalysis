library(rstatic)
source("example/simple.R")
z = mkGlobalsLocal(f, g, main)
z = mkGlobalsLocal(.funs = list(f, g, main))
