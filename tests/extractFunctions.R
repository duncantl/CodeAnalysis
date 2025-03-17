# FIX XXXXXX
library(CodeAnalysis)
fns = getFunctionDefs("nested2.R")
ex = lapply(fns, extractFunctions)

stopifnot(all(sapply(ex, length) == 2))

stopifnot(all(sapply(ex, function(x) length(findCallsTo(x$newFun, "function"))) == 0))

# 3rd doesn't match. Why????
fns2 = lapply(fns, function(x) findCallsTo(x, "function"))
mapply(function(ex, p) length(p) == length(ex$nested),
       ex, fns2)
