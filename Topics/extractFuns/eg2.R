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

e = new.env()
invisible(sapply(o$externalFunctions, function(f) eval(f, e)))


# Now that we have these functions, we find out which global variables
# these refer to. 

globals = lapply(ls(e), function(var) codetools::findGlobals(get(var, e), FALSE))
names(globals) = ls(e)

w = sapply(globals, function(x) length(x$variables)) >  0
sapply(globals[w], `[[`, "variables")


# So we can now use mkGlobalLocals() to rewrite the functions to add the global
# variables as

# mkGlobalLocals()

################################
#
# For reasons totally unrelated to this example, let's find the
# global variables in these functions

intrinsics = c("{", "(", "[", "if", "return", "=", "<-") 

setdiff(codetools::findGlobals(e$KepCalc), intrinsics)
# [1] "-"   "*"   "/"   "+"   ">"   "max" "min"
# So only math operations and simple min and max.
# So we could translate this to C, if we knew the types of the inputs,
# i.e.  Krp[i] where Krp is a numeric vector.


setdiff(unlist(globals[grep("Calc", ls(e), value = TRUE)]), c(intrinsics, ls(e)))
# Mix of variables and functions.
# The functions include paste0, which, tail, nrow, length, vector
