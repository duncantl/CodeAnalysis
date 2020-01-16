removeFromBody =
function(b, index)
{
  b$body = b$body[- index]
}

collectRemoveFun =
function(body)
{
    funcs = list()
    function(expr, i) {

           #XXX This should check that the body of the function does not call <<-
        if(is(expr, "Assign") && is(expr$read, "Function") 
           && is(expr$write, "Symbol")) {
            # collect this
            # funcs <<- append(funcs, expr)
            funcs[[ expr$write$value ]] <<- expr
            # Remove from the body
            removeFromBody(body, i)
        }
    }
}



extractFunctions =
function(fun)
{
    b = to_ast(body(fun))
    col = collectRemoveFun(b)
#browser()
    # Note we go from last to first so that if we remove an element
    # this doesn't change the index of the next element.
    mapply(col, b, rev(seq(along = b$body)))

    funcs = lapply(environment(col)$funcs, as_language)
    body(fun) = as_language(b)
    list(fun = fun, externalFunctions = funcs)
}



findFunctions =
    #
    # Find the functions defined in the body of the given function
    #
function(fun)
{
    if(is.function(fun)) #!!! deal with default values for parameter
        fun = body(fun)

    fun = to_ast(fun)
    i = find_nodes(fun, is, "Function")
    lapply(i, function(x) fun[[x]])
}


findSuperAssignments =
    #
    #  Within a function find non-local assignments, i.e. <<- 
    #
function(fun)
{
    fun =  to_ast(fun)
    i = find_nodes(fun, is, "SuperAssign")
    lapply(i, function(x) fun[[x]])
}
