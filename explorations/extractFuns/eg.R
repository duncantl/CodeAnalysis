source("funs.R")

f = tools::summarize_CRAN_check_status
b = to_ast(body(f))
col = collectRemoveFun(b)

# Note we go from last for first so that if we remove an element
# this doesn't change the index of the next element.
invisible(mapply(col, b$body, rev(seq(along = b$body))))

funs = environment(col)$funcs
sapply(funs, to_r)

sapply(funs, function(x) eval(to_r(x), globalenv()))

body(f) = to_r(b)
f
g = codetools::findGlobals(f, FALSE)
