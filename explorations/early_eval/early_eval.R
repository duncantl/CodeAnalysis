library(CodeDepends)


# I'll think of a better name for this once I figure out what exactly it
# should do.
eval2 = function(expr, env)
{

    expr = as.expression(expr)
    info = CodeDepends::getInputs(expr)
    used = c(info@inputs, names(info@functions))

    # TODO: Evaluate sequentially using
    # lapply(expr, CodeDepends::getInputs)

    available = used %in% ls(env)

    # The simplest thing is to just fail if a variable doesn't exist in the
    # environment. Early evaluation is more sophisticated.
    if(all(available)) {
        return(eval(expr, env))
    } else {
        notfound = paste(used[!available], collapse = " ")
        stop("Variables not found: ", notfound)
    }
}
