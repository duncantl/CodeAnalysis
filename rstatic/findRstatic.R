library(CodeAnalysis)

# parse all the files in CodeAnalysis/R
rf = list.files("R", pattern = "\\.R$", full.names = TRUE)
p = lapply(rf, parse)

# Get all calls
k = unlist(lapply(p, findCallsTo))
length(k)

table(sapply(k, length))
table(sapply(k, function(x) class(x[[1]])))
direct = sapply(k, function(x) is.name(x[[1]]))
k2 = k[direct]


# The non-direct calls might be :: or :::  and also $
ind = sapply(k[!direct], function(x) deparse(x[[1]]))
ns = gsub("::.*", "", grep("::", ind, value = TRUE))
table(ns)
#CodeDepends   codetools      igraph    parallel     rstatic       utils         XML 
#          2           2           3           1          10           1           3 


isNs = sapply(k, isCallTo, c("::", ":::"))
k[isNs]

ns = structure(sapply(k[isNs], function(x) deparse(x[[3]])),
               names = sapply(k[isNs], function(x) deparse(x[[2]])))

# for the direct calls, get the name of the function is being called.
fns = sapply(k2, function(x) deparse(x[[1]]))

rstaticObjs = ls(getNamespace("rstatic"), all = TRUE)
w = fns %in% rstaticObjs
tt = table(fns[w])
names(tt)

funs = getFunctionDefs("R")

g = lapply(funs, getGlobals)

# Detect false positives, i.e.,
# when there is a function CodeAnalysis that would be called instead of one in rstatic
#
# Get the rstatic::foo
#wr = sapply(g, \(x) any(grep("rstatic::", x$functions)))

# local functions 
exFuns = c(names(funs), ls("package:base"))
# now find the names of the functions called each of the functions  that are 
rstFuns = sapply(g, function(x) c(x$functions[ x$functions %in% setdiff(rstaticObjs, exFuns) ],
                                  grep("rstatic::", x$functions, value = TRUE)))
rstFuns = rstFuns[sapply(rstFuns, length) > 0]


# which of these functions are called by other functions
gr = callGraph(funs)

isCalled = names(rstFuns) %in% gr$called
exported = names(rstFuns) %in% getNamespaceExports("CodeAnalysis")


# 37 functions
# What functions call these functions that call rstatic functions.

rfns = names(rstFuns)
while(TRUE) {
    ww <- sapply(g, function(x) any(x$functions %in% rfns))
    add = setdiff(names(g)[ww], rfns)
    if(length(add) == 0)
        break
    rfns = c(rfns, add)
}

we = rfns %in% getNamespaceExports("CodeAnalysis")


ww =  sapply(g, function(x) any(x$functions %in% names(rstFuns)))
z = lapply(g, function(x) any(x$functions %in% names(rstFuns)))
z = z[sapply(z, length) > 0]
# library(rstatic)



################################################

funs2 = getFunctionDefs("R")
dollar = lapply(funs2, findCallsTo, '$')
w = sapply(dollar, length) > 0
dollar = dollar[w]
#d3 = lapply(dollar, function(x) sapply(x, function(x) as.character(x[[3]])))

fields = c("parent", "contents", "target", "write", "read", "body", "exit", 
           "condition", "variable", "iterator", "condition", "args", "fn", 
           "write", "read", "value", "ssa_number", "namespace_fn", "default", 
           "params", "body", "fn", "value")


d3 = lapply(dollar, function(x) x[sapply(x, function(x) as.character(x[[3]]) %in% fields)])
d3 = d3[sapply(d3, length) > 0]

called2 = names(d3) %in% gr$called
exported2 = names(d3) %in% getNamespaceExports("CodeAnalysis")

message("rstatic field accessors")
# Exported functions that call rstatic functions
message("\n# called and exported\n")
mdList(unique(c(names(rstFuns)[isCalled & exported ],
          names(d3)[called2 & exported2])))

message("\n# called and not exported\n")
# Not exported function that call rstatic functions and that are called
mdList(unique(c(names(rstFuns)[isCalled & !exported],
                names(d3)[called2 & !exported2])))

message("\n# not called but exported\n")
# Not called but exported
mdList(unique(c(names(rstFuns)[!isCalled & exported],
                mdList(names(d3)[!called2 & exported2]))))

message("\n# not called and not exported\n")
# Not called and not exported
mdList(unique(c(names(rstFuns)[!isCalled & !exported],
                names(d3)[!called2 & !exported2])))


if(FALSE) {
    # Find the fields in the rstatic R6 AST classes.
    rstaticDir = "~/Theses/NickUlle/rstatic/R/"
    tmp = parse(file.path(rstaticDir, "code_objects_ast.R"))
    cdefs = findCallsTo(tmp, "R6::R6Class")
    names(cdefs) = sapply(cdefs, \(x) x[[2]])

    getR6FieldNames = function(def) {
        w = sapply(def$public, is.null)
        names(def$public)[w]
    }

    fields = lapply(cdefs, getR6FieldNames)
    fields = unlist(fields)
    fields = gsub("^\\.", "", unname(fields))
}
