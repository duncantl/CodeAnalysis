# evaluates to what type - not an explicit return but can handle that too in as much as we can handle types at all
# in this narrow context.
# The context is that we are given a call during the walkCode().

evalsToFunction =
function(x)
{
    if( isCallTo(x, "function") )
        return(TRUE)

    !is.na(ty <- getReturnType(x)) && ty == "function"
}


getReturnType =
function(x)    
{
    if(isLiteral(x))
        return(literalType(x))

    if(isCallTo(x, "function"))
        return("function")
    
    if(isCallTo(x, "{")) {
        y = as.list(x)[-1]
        if(!is.na(ty <- getReturnType(y[[length(y)]])))
            return(ty)

        # now need to look at the earlier calls in this {.
        # Missing context of calls before that. See if we can get those passed
        # to this during the walkCode()
    }

    if(isCallTo(x, "if")) {
        # if call to "if", return the return types from all branches.

    }

    # for while and for and repeat
    
    # if it is a call to a function that we can find()/getAnywhere(), we could
    # try to do type analysis on it.

    
    NA 
}
