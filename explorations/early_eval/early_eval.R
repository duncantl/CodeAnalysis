library(codetools)

early_eval = function(expr)
{
    expr = as.expression(expr)
}


safe_funcs = c("=", "<-", "c", ":", "$", "[", "[[", "list", "seq")


mini = new.env(parent = emptyenv())

# Nothing here yet:
ls(mini)

# This works, discovers + in base
eval(1 + 2, envir = mini)

# Same
local(1 + 2)

# And it finds this in the stats package
eval(median(c(1, 2)), envir = mini)

# Then I need to modify the search path.
# docs say that it's not possible to detach base.
