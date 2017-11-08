# Testing
############################################################
library(rstatic)
library(CodeAnalysis)

source("loop_to_apply.R")
code = parse("loops.R")

ex1 = to_ast(code[1:2])

loop_to_sapply(ex1$body[[2]])

ex1b = to_ast(code[[5]])

# TODO: fix below use case:
loop_to_sapply(ex1b)

# True iterative case
ex3 = to_ast(code[[16]])
loop_to_sapply(ex3)
