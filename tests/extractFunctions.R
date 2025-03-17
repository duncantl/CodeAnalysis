library(CodeAnalysis)
fns = getFunctionDefs("nested2.R")
ex = lapply(fns, extractFunctions)

stopifnot(all(sapply(ex, length) == 2))

stopifnot(all(sapply(ex, function(x) length(findCallsTo(x$newFun, "function"))) == 0))

# 3rd - h - doesn't match. Why????
# 
fns2 = lapply(fns, findNamedFunctions)

nex = sapply(ex, function(x) length(x$nested))
nog = sapply(fns2, length)
stopifnot(all(nex == nog))

