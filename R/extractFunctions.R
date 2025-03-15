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
    # or in any code.
    #
function(code)
    findCallsTo(fun, "function")



findSuperAssignments =
    #
    #  Within a function or any code find non-local assignments, i.e. <<- 
    #
function(fun, ...)
{
    # could also be findCallsTo(, "<<-")
    findAssignsTo(fun, assignmentOps = "<<-", ...)
}
