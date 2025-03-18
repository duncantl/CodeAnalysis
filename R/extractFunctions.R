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
    nested = findNamedFunctions(fun)
    fun2 = removeNamedFunctionDefs(fun)
    list(newFun = fun2, nested = nested)
}

removeNamedFunctionDefs =
function(fun)    
{
    # XXX  this doesn't handle a <- b <- function()...
    # Ends up with a <-  and then NULL and the assignment language object has length 2 but displays as a <- NULL.
    # See tools:::get_CITATION_entry_fields  and FOO1 <- FOO2 <- function().
    #
    # XXX  var = if(...)  function() ... else function( ) ... 
    #  also tools::check_doi_db 
    #
    rw = genRemoveCode(isNamedFunctionAssign)
    w = mkModifyCodeWalker(rw, FALSE)
    walkCode(fun, w)        
}


findFunctions =
    #
    # Find the functions defined in the body of the given function
    # or in any code.
    # Either assigned or anonymous.
    #
function(code)
    findCallsTo(code, "function")


isNamedFunctionAssign =
    #
    # Do we grow this (or another function) to find
    #  a <- b <- function
    # and
    #  a = if(...) function() else function()
    #
function(x, ...)
{
    w0 = isSimpleAssignTo(x)
    if(!w0)
        return(FALSE)
    
    w =  w0 && isCallTo(x[[3]], "function")
    if(w)  
        return(TRUE)

    rhs = x[[3]]
    w = isCallTo(rhs, "if") && all(sapply(rhs[3:4], evalsToFunction))
    if(w)
        return(TRUE)

    FALSE
}

findAssignedFunctions = findNamedFunctions =
    # This version finds only function() ...  which are assigned to
    # a simple variable.
    # So ignore, e.g., fns$x = function()...
function(fun)
{
    asg = findAssignsTo(fun)
    if(length(asg) == 0)
        return(list())
    
    isFun = sapply(asg, isNamedFunctionAssign)

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
