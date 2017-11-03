kode = parse("../extractFuns/example/FAO56_dualcropcoeff.R")

isFunction = function(x) is(x, "<-") && is.call(x[[3]]) && x[[3]][[1]] == "function"
isFun = sapply(kode, isFunction)
table(isFun)

# Just one. That is what we are looking for
f = kode[[which(isFun)]]

f[[2]]
fun = eval(f[[3]])
sapply(body(fun), isFunction)

o = extractFunctions(fun)




