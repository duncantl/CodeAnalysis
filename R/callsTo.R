# find the calls to given functins

mkCallWalkerPred =
    #
function(pred, ...)
{
    calls = list()

    leaf = function(x, w, ...) {
        ty = typeof(x)
        if(ty == "pairlist" || ty == "expression" || ty == "list") {
            lapply(x, walkCode, w)
            return(NULL)
        } else if(ty == "closure") {
            walkCode(formals(x), w) # lapply(formals(x), walkCode, w)
            walkCode(body(x), w)
            return(NULL)
        } 
    }
    
    call = function(x, w) {

        isName = is.name(x[[1]])
        if(pred(x, isName, ...)) 
            calls[[length(calls) + 1L]] <<- x

        els = as.list(x)
        if(isName && as.character(x[[1]]) %in% c(".Internal", ".Primitive")) 
            els = els[-2]
        
        for (ee in els) {
            if (!missing(ee))
                walkCode(ee, w)
        }
    }    

    list(handler = function(x, w) NULL, leaf = leaf, call = call, ans = function() calls )
}

mkCallWalker =
function(funNames)
{
    isFunNamesStrings = is.character(funNames)
    ok = function(x, isName, ...) {
            if(length(funNames) == 0)
                return(TRUE)
            
            if(!isFunNamesStrings) 
                return(any(sapply(funNames, identical, x[[1]])))
             
            (isName && (as.character(x[[1]]) %in% funNames)) ||
               ( is.call(x[[1]]) && is.name(x[[1]][[1]]) &&
                   as.character(x[[1]][[1]]) %in% c("::", ":::") && deparse(x[[1]]) %in% funNames ) 
          }

    mkCallWalkerPred(ok)
}



findCallsTo =
    # How is this related to findCallsToFunctions()?
    #
    #
    # f = getAnywhere("is.nloptr")$objs[[1]]
    # findCallsTo(f, list(parse(text = "x$eval_f")[[1]]))
    # findCallsTo(f, "x$eval_f")
    # findCallsTo(f, "x$eval_f", parse = TRUE)
    #
function(code, funNames = character(), walker = mkCallWalker(funNames), parse = any(!sapply(funNames, is.name)))
{
    if(parse) 
        funNames = lapply(funNames, function(x) parse(text = x)[[1]])
    
    if(is(code, "getAnywhere")) {
        if(length(code$objs) > 1)
            stop("more than one function in the getAnywhere object")
        code = code$objs[[1]]
    } else if(is.environment(code)) 
        code = as.list.environment(code, TRUE)

    
    walkCode(code, walker)
    walker$ans()
}

