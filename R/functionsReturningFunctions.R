if(FALSE) {
    # To define these functions
    # eval(parse("functionsReturningFunctions.R")[[1]][[3]])
    # To run tests
    # lapply(as.list(parse("functionsReturningFunctions.R")[[2]][[3]])[-1], eval)
    
    lik = function(x)
        function(mu, sd)
            prod(dnorm(x, mu, sd))

    f2 = function(x)
        Vectorize(bar)

    f3 = function(x) {
        if(length(x) > 3) {
            y = x[-(1:3)]
            function(z)
                prod(z + y)
        } else
            sum
    }

    f3.5 = function(x) {
        sum = 2
        if(length(x) > 3) {
            y = x[-(1:3)]
            function(z)
                prod(z + y)
        } else
            sum
    }    

    f4 = function(x) {
        f = function(x, y) x+y
        attr(f, "class") = "SpecialFunction"
        f
    }

    f5 = function(x) {
        f = function() {}
        formals(f)$a = 10
        formals(f)$b = 2
        body(f)[[2]] = quote(a^b)
        attr(f, "class") = "SpecialFunction"
        f
    }
}
if(FALSE) {
    returnsFunction(lik)
    returnsFunction(f2, functionsReturningFunctions = "Vectorize")
    returnsFunction(f3)  # inline & sum
    returnsFunction(f3.5)  #just the inline definition
    returnsFunction(f4)
    returnsFunction(f5)
}

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
    ans = lapply(ret, resolveVar, fun)

    if(length(ans)) {
        ans[ sapply(ans, definesFunction, fun, recursive, envir, functionsReturningFunctions) ]
    } else
        ans
}

resolveVar =
function(ex, fun)
{
    if(is.name(ex)) {
        # want complex assignments as well as simple intial assignment
        tmp = findAssignsTo(fun, as.character(ex))
        if(length(tmp)) {
            # Combine the earlier assignments and add as attributes ?
            a = tmp[[length(tmp)]]
            if(length(tmp) > 1)
                attributes(a, "intermediateAssignments") = tmp[-length(tmp)]
            # Assuming var = value
            a[[3]]
        } else
            ex
    } else
        ex
}


definesFunction =
    #
    #
    #    
function(x, fun, recursive = FALSE, envir = globalenv(), functionsReturningFunctions = character())
{
    if(isCallTo(x, "function"))
        return(TRUE)

    if(is.name(x)) {
        # check to see if this name is assigned in the function.
        # If so, is it a function.
        # If not, resolve the name in envir and see if a function.

        id = as.character(x)
        defs = findAssignsTo(fun, id)
        if(length(defs) == 0)
            return(exists(id, envir, mode = "function"))

        browser()
    }

    if(is.name(x[[1]])) {
        fn = as.character(x[[1]])
        if(fn %in% functionsReturningFunctions)  # length(functionsReturningFunctions) &&
            return(TRUE)
    
        #XXX implement recursive
        # What if call foo$bar()
        fn2 = get(fn, envir, mode = "function")
        return(length(returnsFunction(fn2, recursive, envir, functionsReturningFunctions)) > 0)
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
        lapply(ret, function(x) if(isCallTo(x, "return")) x[[1]] else x)
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


