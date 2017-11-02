library(rstatic)

code = parse("loops.R")

ex1 = to_ast(code[1:2])

# Testing for a very specific pattern inside the for loop

forloop = ex1$body[[2]]


replacer = forloop$body$body[[1]]$read

# test index assignment
if( replacer$fn$basename == "[<-" ) "continue"

# test RHS subset
ss = replacer$args[[3]]$args[[1]]
if( is(ss, "Subset") ) "continue"

# index, ivar, and subset function index should all be the same
if( replacer$args[[2]]$basename == forloop$ivar$basename &&
    forloop$ivar$basename == ss$args[[2]]$basename ) "continue"

# Passes checks, convert to sapply
fun = replacer$args[[3]]$fn

lhs = replacer$args[[1]]

arg = ss$args[[1]]

substitute(lhs <- sapply(fun, arg))
