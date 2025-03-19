bs = getNamespace("base")
be = as.list(bs, all.names = TRUE)
length(be)
w = sapply(be, is.function)
table(w)
be = be[w]

names(be)

pnames = lapply(be, function(x) names(formals(x)))
w2 =sapply(pnames, function(x) any(c("f", "FUN", "fun") %in% x))

ind = getIndirectCallFunList()
setdiff(names(pnames)[w2], names(ind))


# Vectorize
# unsplit
# reg.finalizer
# addTaskCallback
# lazyLoadDBexec - fun, filter


#### methods
# split
# by



# debug, isdebugged, debugonce, environment


#
cp = lapply(be, findCallsParam)
w3 = sapply(cp, length) > 0
table(w3)

names(cp)[w3]

v = sort(setdiff(names(cp)[w3], names(ind)))

cat(sprintf("%s - %s", v, sapply(cp[v], paste, collapse = ", ")), sep = "\n")


# We get false positives.
# e.g., parse has a parameter named file and it calls file(). But that is the function file() that will be found
#  in base.
# Same with factor and its param levels and calls to levels().

# FIX   <<<<<<<<<<<<<<<
# Also getting .Internal for getAllConnections.  And ! and if and { and <-
# If no parameters, then asking it to find all calls.

# So let's be overly aggressive and remove those where there is a function in base with the same name as the parameter that appears to be called.
cp2 = lapply(cp, function(x) setdiff(x, names(be)))
w4 = sapply(cp2, length) > 0
names(w4)[w4]


tt = dsort(table(unlist(cp[!w4])))


