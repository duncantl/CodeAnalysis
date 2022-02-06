# find the calls to given functins

mkCallWalker =
function(funNames)
{
    calls = list()

    leaf = function(x, w, ...) {
        ty = typeof(x)
        if(ty == "pairlist") {
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
        if(isName && (length(funNames) == 0 || as.character(x[[1]]) %in% funNames)) {
            calls[[length(calls) + 1L]] <<- x
        } else if(is.call(x[[1]]) && is.name(x[[1]][[1]]) && as.character(x[[1]][[1]]) %in% c("::", ":::") && deparse(x[[1]]) %in% funNames) {
                calls[[length(calls) + 1L]] <<- x
        }
        

        els = as.list(x)
        if(isName && as.character(x[[1]]) %in% c(".Internal", ".Primitive")) 
            els = els[-2]

        
        for (ee in els)
            if (!missing(ee))
                walkCode(ee, w)
    }    

    list(handler = function(x, w) NULL, leaf = leaf, call = call, ans = function() calls )
}

findCallsTo =
function(code, funNames = character(), walker = mkCallWalker(funNames))
{
    walkCode(code, walker)
    walker$ans()
}
