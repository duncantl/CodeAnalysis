library(CodeAnalysis)
ns = getNamespace("tools")
tfns = as.list(ns, all.names = TRUE)
w = sapply(tfns, is.function)
nfns = lapply(tfns[w], getFunctionDefs)
ex = lapply(tfns[w], extractFunctions)

# Fixed now.
# # Expect 1 error currently  FOO1 <- FOO2 <- function()...
# err = sapply(ex, inherits, 'try-error')
# w2 = which(w)[!err]

d = data.frame(numTerms = sapply(tfns, numTerms),
               sizeReducedFunction = sapply(ex, function(x) numTerms(x$new)),
               numExtractedFunctions = sapply(ex, function(x) length(x$nested)),
               sizeExtractedFunctions = sapply(ex, function(x) if(length(x$nested)) sum(sapply(x$nested, numTerms)) else 0L),
               # working with nfns is not really relevant as this includes anonymous functions.
               numFunctions = sapply(nfns, length),               
               sizeNestedFunctions = sapply(nfns, function(x) if(length(x)) sum(sapply(x, numTerms)) else 0L)
               )

tmp = subset(d, numFunctions == 0)
stopifnot(all(tmp$sizeNestedFunctions == 0))
stopifnot(all(tmp$sizeExtractedFunctions == 0))


d2 = subset(d, numExtractedFunctions > 0)

d2$back = with(d2, sizeReducedFunction + sizeExtractedFunctions + numExtractedFunctions*2 )

d2$diff = with(d2, numTerms - back)
summary(d2$diff)

w3 = d2$diff != 0
table(w3)

d3 = d2[w3, ]
d3[order(d3$numTerms), ]

# format.codocClasses &  fetchRdDB
#                        numTerms sizeReducedFunction numExtractedFunctions sizeExtractedFunctions numFunctions sizeNestedFunctions  back  diff
# format.codocClasses          70                   9                     3                     99            1                  57   114   -44
# fetchRdDB                    88                  14                     2                     84            1                  70   102   -14

v = "format.codocClasses"
o = tfns[[v]]
n = ex[[v]]$new
stopifnot(identical(formals(o), formals(n)))
ob = as.list(body(o))
nb = as.list(body(n))
length(ob)



# Check for multiple nested named functions
ids = rownames(d2)[w3]
nn = lapply(ex[ids], function(x) sapply(x$nested, function(x) length(findNamedFunctions(x))))
stopifnot(all(sapply(nn, max) > 0))
# all have at least one nested name function.
