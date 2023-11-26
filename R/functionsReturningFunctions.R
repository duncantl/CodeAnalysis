returnsFunction =
    #
    # Takes a function and returns a logical value indicating
    # whether this function returns a function.  It can return different types
    # If any is FALSE, then all return values must be functions to return TRUE.
    # Otherwise, if any return value is a function, we return TRUE.
    #
    #
    # recursive means look at returns that are calls and follow those functions
    # envir is where to look for these functions
    # returnsFunctionNames is a character vector of functions that we know may/do return functions.
    #  I think we have a start of the names of such functions.
    #
function(fun, recursive = FALSE, envir = globalenv(),
         functionsReturningFunctions = character(), any = FALSE)
{
    ret = getReturnValues(fun)
    #    value = lapply(ret, function(x) if(is.call(x)) x[[2]] else x)

    # if a return value is a symbol, go find where it was last assigned to get that value.
    # XXX need to deal with cases when the variable has multiple replacements,
    #   e.g., x = foo(), x$e = val, ... then return(x)
    #
    # In the case of Vectorize() returning a (function(){ function() ...})()
    # leave it to definesFunction to see if this evaluates to a function.
    #
    ans = lapply(ret, resolveVar, fun)

    if(length(ans)) {
        w = sapply(ans, definesFunction, fun, recursive, envir, functionsReturningFunctions) 
        ans[ !is.na(w) & w ]
    } else
        ans
}

resolveVar =
    #
    # given a expression (ex),  if it is a name/symbol,
    # find where it is assigned and get the RHS, i.e., its value.
    # This skips updates to that variable.
    #
    # If not a name, return ex.
    #
function(ex, fun)
{
    if(is.name(ex)) {
        # want complex assignments as well as simple intial assignment
        tmp = findAssignsTo(fun, as.character(ex))
        if(length(tmp)) {
            # Combine the earlier assignments and add as attributes ?
            direct = sapply(tmp, isSimpleAssignTo, character())
            if(!any(direct)) {
                # could be a parameter.
                if(!isParameter(ex, fun)) 
                   browser()
            }
            
            i = which.max(direct)
            # Since a simple assignment, has to be of form var = value
            a = tmp[[ i ]][[3]]
            
            if(i < length(tmp) && !is.null(a) && !is.name(a)) #XXX figure out what to do here if symbol.
                attr(a, "intermediateAssignments") = tmp[ (i+1):length(tmp) ]
            a
        } else
            ex
    } else
        ex
}



definesFunction =
    #
    # Does the language object x evaluate to a function.
    #    
function(x, fun, recursive = FALSE, envir = globalenv(), functionsReturningFunctions = character())
{
    if(isCallTo(x, "function"))
        return(TRUE)

    if(is.name(x)) {
        # Already done in resolveVar().
        # check to see if this name is assigned in the function.
        # If so, is it a function.
        # If not, resolve the name in envir and see if a function.

        id = as.character(x)
        defs = findAssignsTo(fun, id)
        if(length(defs) == 0) {

            if(id %in% names(formals(fun))) {
                # ? Can't assign this and then use in is.name
                #  val = formals(fun)[[id]]

                # is.name(formals(fun)[[id]]) && as.character(formals(fun)[[id]]) != ""
                if(! isSymbol(formals(fun)[[id]], "" )) 
                    # Want to signal that this is a parameter and not to look in the fun.
                   return( definesFunction(formals(fun)[[id]], fun, recursive, envir, functionsReturningFunctions) )

                
                # structure(val, class = "ParameterValue"))
                return(NA)
            }

            return(exists(id, envir, mode = "function"))
        }
    }

    if(is.call(x) && is.name(x[[1]])) {
        fn = as.character(x[[1]])
        if(fn %in% functionsReturningFunctions)  # length(functionsReturningFunctions) &&
            return(TRUE)
    
        #XXX implement recursive
        # What if call foo$bar()
        if(recursive) {
            fn2 = get(fn, envir, mode = "function")
            # probably need to process the actual call, i.e., provide the arguments
            # so that can analyze, e.g., branching in fn2.
            return(length(returnsFunction(fn2, recursive, envir, functionsReturningFunctions)) > 0)
        } else
            return(FALSE)
    }

    if(is.call(x)) {
        fn2 = x[[1]]
        if(isNamespaceAccess(fn2)) {
            if(length(functionsReturningFunctions))
                return( any(c(deparse(fn2), as.character(fn2[[3]])) %in% functionsReturningFunctions))
            else if(recursive) {
                return(length(returnsFunction(eval(fn2, envir), recursive = recursive, envir = envir,
                                       functionsReturningFunctions = functionsReturningFunctions)) > 0)
            } else
                return(NA) #??? e.g. methods::slot
        }
        
#   browser()
        # So function being called in x is the result of a call itself.
        if(isCallTo(fn2, "("))
            fn2 = fn2[[2]]

        if(isCallTo(fn2, "function")) {
           return(length(returnsFunction(eval(fn2), envir = envir, functionsReturningFunctions = functionsReturningFunctions)) > 0)
        }
        
    }
    
    
    return(FALSE)
}


getReturnValues =
function(fun, rmReturn = TRUE)
{
    ret = findCallsTo(fun, "return")
    
    # Now get the last value
    lval = getLastValue(body(fun))
    
    # If the last value is a call to return() it will already be in ret.
    if(!isCallTo(lval, "return"))
        ret = c(ret, lval)

    if(rmReturn)
        # get the value of the explicit return()
        lapply(ret, function(x) if(isCallTo(x, "return")) {
                                    if(length(x) > 1)
                                        x[[2]]
                                    else NULL
                                } else x
               )
    else
        ret
}

getLastValue =
    # b is the body body of the function
function(b)
{
    lval = if(class(b) == "{")
               b[[length(b)]]
           else
               b
    if(isCallTo(lval, "if")) 
        # get the last value of each branch
        lval = getIfLastValues(lval)

    lval
}

getIfLastValues =
    #
    # get last values from  if statement blocks.
    #
    # Takes a call to if and its recursive else if and else
    # gets the last value from each if/else block.
    # This works recursively and gets the value from all the if and else-if and else clauses
    #
    # e = quote(if(x) a else if(y) b else z)
    #
function(x)
{
    a = getLastValue(x[[3]])
    if(length(x) == 4)
        c(a, getLastValue(x[[4]]))
    else
        a
}


