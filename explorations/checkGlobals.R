z = findGlobals(cb, FALSE)
stopifnot(identical(z, list(functions = "foo", vars = "bob")))


z = findGlobals(do, FALSE)
stopifnot(identical(z, list(functions = c("lapply", "table", "sapply", "sort", "mapply", 
"f", ":", ":", "apply", "order"), vars = "m")))

z = findGlobals(ns, FALSE)
stopifnot(identical(z, list(functions = c("base::seq", "foo", "+"), vars = character(0))))


z = findGlobals(nested0, FALSE)
stopifnot(identical(z, list(functions = c("sapply", "g", "+"), vars = character())))

z = findGlobals(nested, FALSE)
stopifnot(identical(z, list(functions = c("sapply", "g", "+"), vars = "alpha")))


z = findGlobals(nested2, FALSE)
stopifnot(identical(z, list(functions = c("sapply", "g", "+"), vars = "beta")))

z = findGlobals(nested3, FALSE)
stopifnot(identical(z, list(functions = c("sapply", "g"), vars = "beta")))


z = findGlobals(whileLoop, FALSE)
stopifnot(identical(z, list(functions = c(">", "f", "+"), vars = c("a", "a", "b"))))

