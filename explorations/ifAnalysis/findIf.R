library(rstatic)
source("ifFuns.R")

recPackages = list.files(.libPaths()[2])

recPkgsIfs = lapply(recPackages, function(p) { print(p); try(findIf(p))})
names(recPkgsIfs) = recPackages

e = sapply(recPkgsIfs, is, 'try-error')
recPackages[e]
# tools and translations failed.

recPkgsIfs = recPkgsIfs[!e]




###############
b = findIf("base")
b2 = unlist(b, recursive = FALSE)

noElse = sapply(b2, function(x) length(x$false$contents)) == 0
isStop = sapply(b2, function(x) length(x$body$contents) == 1 && is(x$body$contents[[1]], "Call") && is_symbol(x$body$contents[[1]]$fn, "stop"))

i = (noElse | isStop)
table(i)
b3 = b2[!i]

isCondLiteral = sapply(b2, function(x)  is(x$condition, "Logical"))
# none

vals = lapply(b3, getIfValue)


# Look at assignments and check if the if-else are both assigning to the same variable.
w = sapply(vals, function(x) all(sapply(x, is, "Assignment")))
w2 = sapply(vals[w], function(x) length(unique(sapply(x, function(x) x$write$value)))) > 1

# This just looks at the last value in the if or else body. The same
# variable may be assigned elsewhere in these blocks, e.g. Summary.difftime where
# args is assigned in both, but also units in the else clause.








