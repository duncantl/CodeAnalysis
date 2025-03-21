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
    if(length(nested)) {
        fun2 = removeNamedFunctionDefs(fun, names(nested))
        list(newFun = fun2, nested = nested)
    } else
        list(newFun = fun, nested = list())
    
}


removeNamedFunctionDefs =
function(fun, names = character())    
{
    # XXX  this doesn't handle a <- b <- function()...
    # Ends up with a <-  and then NULL and the assignment language object has length 2 but displays as a <- NULL.
    # See tools:::get_CITATION_entry_fields  and FOO1 <- FOO2 <- function().
    #
    # XXX  var = if(...)  function() ... else function( ) ... 
    #  also tools::check_doi_db 
    #
    p = isNamedFunctionAssign
    formals(p)$names = names
    rw = genRemoveCode(p)
    w = mkModifyCodeWalker(rw) # , FALSE)
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
    #
    # CodeAnalysis:::isNamedFunctionAssign(quote( x <- function(x) x+1))
    # CodeAnalysis:::isNamedFunctionAssign(quote( x <- if(a) function(x) x+1 else function(y) y/2))    
    #
function(x, names = character(), ...)
{
    w0 = isSimpleAssignTo(x)
    if(!w0)
        return(FALSE)
    
    w =  w0 && isCallTo(x[[3]], "function")
    if(w  && (length(names) == 0 || as.character(x[[2]]) %in% names))
        return(TRUE)

    rhs = x[[3]]
    w = isCallTo(rhs, "if") && evalsToFunction(rhs) 
    if(w && (length(names) == 0 || as.character(x[[2]]) %in% names))
        return(TRUE)

    FALSE
}

findNamedFunctions =
    # This version finds only function() ...  which are assigned to a simple variable.
    # So it ignores, e.g., fns$x = function()...
    # Could make complex = TRUE/FALSE a parameter
    #
    #
    # h = function() { FOO1 <- FOO2 <- function() 1}
    # f = function() { FOO1 <- FOO2 <- function() 1; g = function(x) x + 1; g(2)}
    # findNamedFunctions(h)
    # findNamedFunctions(g)    
    #
    #
function(fun, skipChained = FALSE)
{
    asg = findAssignsTo(fun, complex = FALSE)
    if(length(asg) == 0)
        return(list())
    
    isFun = sapply(asg, isNamedFunctionAssign)

    #XXX
    # Deal with chained assignments a <- b <- function()...
    # check if RHS is also a simple assign of a function
    # and the hopefully it is the next one in the list.  Not definitively a match
    # as could  have a <- b <- function () .... and then another b <- function() ...
    # But really!
    # Check if either assignment (and there could be more than 2)


    if(!skipChained) {
        w = sapply(asg, isChainedAssign)
        isFun = isFun & !w
    }
        
    funs = lapply(asg[isFun], function(x) x[[3]])
    names(funs) = sapply(asg[isFun], function(x) as.character(x[[2]]))
   
    funs
}

AssignmentOps = c("<-", "=", "<<-")

isChainedAssign =
    #
    # can add parameter for simple/complex assignment.
    #
function(x, assignmentOps = AssignmentOps)    
{
    !is.null(attr(x, "chainedAssignmentTo")) ||
      ( isCallTo(x, assignmentOps) && length(x) > 2 && isCallTo(x[[3]], assignmentOps))
}
        
rhs = last =
    #
    # to get the RHS of a sequence of a = b = c = d = ..... = value
    #
    # rhs(quote(a <- b <- c <- d <- rep(NA, length(y))))
    # returns rep(NA, length(y))
    #
function(x, assignmentOps = c("<-", "=", "<<-"))
{
    o = x
    while(isCallTo(x, assignmentOps) && isSymbol(x[[2]]) && length(x) > 2)
        x = x[[3]]

    x
}



findSuperAssignments =
    #
    #  Within a function or any code find non-local assignments, i.e. <<- 
    #
function(fun, ...)
{
    # could also be findCallsTo(, "<<-") since complex = TRUE
    findAssignsTo(fun, assignmentOps = "<<-", ...)
}
