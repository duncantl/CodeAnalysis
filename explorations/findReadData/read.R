source("findReadDataCalls.R")
kk = to_ast(parse("readEg.R"))
defs = getDefinedFuns(kk)
