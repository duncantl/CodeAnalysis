# find the calls to given functins

mkCallWalker =
function(funNames)
{
    calls = list()

    leaf = function(x, w, ...) {
        ty = typeof(x)
        if(ty == "pairlist" || ty == "expression") {
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
        if((isName && (length(funNames) == 0 || as.character(x[[1]]) %in% funNames)) ||
               ( is.call(x[[1]]) && is.name(x[[1]][[1]]) && as.character(x[[1]][[1]]) %in% c("::", ":::") && deparse(x[[1]]) %in% funNames ) ) {
            calls[[length(calls) + 1L]] <<- x
        } 
        
        
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


findCallsTo =
    # How is this related to findCallsToFunctions()?
function(code, funNames = character(), walker = mkCallWalker(funNames))
{
    if(is(code, "getAnywhere")) {
        if(length(code$objs) > 1)
            stop("more than one function in the getAnywhere object")
        code = code$objs[[1]]
    }
    
    
    walkCode(code, walker)
    walker$ans()
}
