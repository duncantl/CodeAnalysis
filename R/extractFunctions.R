# rstatic
if(FALSE)
removeFromBody =
function(b, index)
{
  b$body = b$body[- index]
}

# rstatic
if(FALSE)
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



# rstatic
if(FALSE)
extractFunctions =
    #
    #  Perhaps use indexWalkCode()
    #  or just write a walkCode() walker that removes the functions
    #  and stores them so can return the updated code and the function.
    #  Will look for function definitions in the current expression,
    #  i.e. from the parent from which we will have to remove them.
    #
function(fun)
{
    b = to_ast(body(fun))
    col = collectRemoveFun(b)

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
    findCallsTo(code, "function")


findAssignedFunctions = findNamedFunctions =
    # This version finds only function() ...  which are assigned to
    # a simple variable.
    # So ignore, e.g., fns$x = function()...
function(fun)
{
    asg = findAssignsTo(fun)
    isFun = sapply(asg, function(x) isSimpleAssignTo(x) && isCallTo(x[[3]], "function"))

    funs = lapply(asg[isFun], function(x) x[[3]])
    names(funs) = sapply(asg[isFun], function(x) as.character(x[[2]]))
   
    funs
}



findSuperAssignments =
    #
    #  Within a function or any code find non-local assignments, i.e. <<- 
    #
function(fun, ...)
{
    # could also be findCallsTo(, "<<-")
    findAssignsTo(fun, assignmentOps = "<<-", ...)
}
