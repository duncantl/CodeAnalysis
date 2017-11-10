early_eval = function(expr)
{
    expr = as.expression(expr)
}


safe_funcs = c("=", "<-", "c", ":", "$", "[", "[[", "list", "seq")


mini = new.env(parent = emptyenv())

# Nothing here yet:
ls(mini)

# Darn, I hoped it wouldn't find + in the base package
eval(1 + 2, envir = mini)

# And it finds this in the stats package
eval(median(c(1, 2)), envir = mini)

# Then I need to modify the search path.
