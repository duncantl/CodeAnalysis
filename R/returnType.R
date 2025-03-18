# evaluates to what type - not an explicit return but can handle that too in as much as we can handle types at all
# in this narrow context.
# The context is that we are given a call during the walkCode().


# evalsToFunction just does the obvious and then calls getReturnType() if that didn't work.
# getReturnType() does a heuristic analysis.
# Need to turn this into some much more robust.
# But for now we are trying to handle the case of
#  f  = if(...) function() ... else function(...) ...
# Which is pretty simple.


# XXX There is an existing function named evalsToFunction in freeVariables.R
evalsToFunction2 =
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


    if(isSimpleAssignTo(x))
        return(getReturnType(x[[3]]))
    
    if(isCallTo(x, "{")) 
        return(getReturnType.brace(x))        

    if(isCallTo(x, "if")) {
        # if call to "if", return the return types from all branches.

        tys = sapply(x[3:4], getReturnType)
        return(unique(tys))
    }

    # for while and for and repeat
    
    # if it is a call to a function that we can find()/getAnywhere(), we could
    # try to do type analysis on it.

    
    NA 
}

getReturnType.brace =
function(x)
{
    y = as.list(x)[-1]
    last = y[[length(y)]]
    if(!is.na(ty <- getReturnType(last)))
        return(ty)

    # now need to look at the earlier calls in this {.
    # Missing context of calls before that. See if we can get those passed
    # to this during the walkCode()

    if(isSymbol(last)) {
        # look for assignments and updates within {, i.e. earlier elements

        last2 = as.character(last)
        
        vals = findAssignsTo(y[ - length(y) ], last2)
        w = sapply(vals, isSimpleAssignTo)
        if(any(w)) {
            i = max(which(w))
            vals = vals[i:length(vals)]
        }
        
        # this should probably be a for() loop and we get the return type
        # of the previous one and update it.
        #         ty = sapply(vals, getReturnType)
        
        ty = getReturnType(vals[[1]])
        if(length(vals) > 1) {
            browser()
            for(v in vals[-1]) {
                # Check if changes type.
                ty = assignType(v, ty, last2)
                if(is.na(ty))
                    break
            }

            return(ty)
        }
    }
}

assignType =
    #
    # x currently expected to be a complex assignment to a variable var
    # type is the current type of var
    # var is a character giving the name.
    #
    #  environment(fn) = value
    #  foo(x) = value
    #  names(x) = value
    #
    #
function(x, type, var)
{
    rhs = x[[2]]
    # is this a simple call fun(var, ...)
    # or a nested sequence of calls h(g(var, ...))
    simple = is.call(rhs) && isSymbol(rhs[[1]]) && isSymbol(rhs[[2]], var)
    if(simple) {

        op = as.character(rhs[[1]])
        ty = switch(op,
                    names =,
                    attr=,
                    length=,
                    environment = type,
                    NA
                    )
        return(ty)
    }

    # can chase down complex calls on rhs   XXX

    # for now just say we don't know the resulting type.
    NA
}
