isParameter = isArgument =
function(p, fun)
{
   p %in% names(formals(fun))
}


isLocalVar = isLocalVariable =
function(p, fun, notParam = TRUE)
{
    asgn = findAssignsTo(fun, p)
    if(length(asgn) == 0)
        return(FALSE)

    if(notParam && isParameter(p, fun))
        return(FALSE)

    return(length(asgn) > 0)
}
