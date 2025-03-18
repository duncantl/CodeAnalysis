library(CodeAnalysis)

# This takes about 11.5 seconds to run.
# There are currently 716 functions in tools.

# Not run directly as this is more of an example and exploration than a test.
# It takes a 
ns = getNamespace("tools")
tfns = as.list(ns, all.names = TRUE)
tfns = tfns[sapply(tfns, is.function)]

ts3 = lapply(tfns, S3Assignments)

w = sapply(ts3, length) > 0
table(w)
o = ts3[!w]

table( sapply(ts3[w], length))
tt = sort(table(unlist(ts3[w])), decreasing = TRUE)

# Not just the classes but all the attributes used.
# Could look at tfns[!w] but we'll look at them all.
st = lapply(tfns, getAttributeNames)
tt2 = sort(table(unlist(st)), decreasing = TRUE)

