# find the calls to specified/target functions

mkCallWalkerPred =
    #
function(pred, ...)
{
    calls = list()

    leaf = function(x, w, ...) {
        ty = typeof(x)
        if(ty %in% c("pairlist", "expression", "list", "language")) {
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
        if(pred(x, isName, ...))  # XXXX This is very interesting as to where the ... comes from
            # the mkCallWalkerPred() or the call function.
            # It has to be the former as call() doesn't have ...
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

isNamespaceAccess =
function(x)
  is.call(x) && isSymbol(x[[1]], c("::", ":::")) 


mkCallWalker =
function(funNames, indirect = character())
{
    isFunNamesStrings = is.character(funNames)
    ok = function(x, isName, ...) {
            if(length(funNames) == 0)
                return(TRUE)
            
            if(!isFunNamesStrings && 
               any(sapply(funNames, identical, x[[1]])))
                return(TRUE)
             
            yes = (isName && (as.character(x[[1]]) %in% funNames)) ||
                    ( is.call(x[[1]]) && isNamespaceAccess(x[[1]]) &&
                     (deparse(x[[1]]) %in% funNames || (is.name(x[[1]][[3]]) && as.character(x[[1]][[3]]) %in% funNames)))

            if(yes)
                return(yes)

            if(length(indirect))
                return(isIndirectCall(x, indirect, funNames, isFunNamesStrings))

            FALSE
          }

    mkCallWalkerPred(ok)
}

isIndirectCall =
function(x, indirects, funNames, isFunNamesStrings)    
{
    if( isSymbol(x[[1]]) && (fn <- as.character(x[[1]])) %in% names(indirects)) {
        argName = indirects[[fn]]
        fun = get(fn) # XXX
        k = match.call(fun, x)
        if(!(argName %in% names(k)))
            return(FALSE)
        
        arg = k[[argName]]
        if(!(is.character(arg) || isSymbol(arg)))
            # XXX  handle pkg::fun or pkg:::fun  - see code in regular predicate and consolidate.
            return(FALSE)  
        
        if(isFunNamesStrings)
            return(as.character(arg) %in% funNames)
        else
            return(any(sapply(funNames, identical, arg)))
    }

    FALSE
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
function(code, funNames = character(),
         indirectCallFuns = IndirectCallFunList,
         walker = mkCallWalker(funNames, indirect = indirectCallFuns),
         parse = any(!sapply(funNames, is.name)))
{
    if(parse) 
        funNames = lapply(funNames, function(x) parse(text = x)[[1]])
    
    if(is(code, "getAnywhere")) {
        if(length(code$objs) > 1)
            stop("more than one function in the getAnywhere object")
        code = code$objs[[1]]
    } else if(is.environment(code)) 
        code = as.list.environment(code, TRUE)

    if(is.logical(indirectCallFuns) && missing(walker)) {
        indirectCallFuns = if(isTRUE(indirectCallFuns))
                               getIndirectCallFunList()
                           else
                               character()
    }
    
    walkCode(code, walker)
    walker$ans()
}



# Used by getGlobals() via call to getIndirectCallFunList().

IndirectCallFunList = c(do.call = "what",
                        match.fun = "FUN",
                        match.call = "def")

IndirectCallFunList[c("apply", "lapply", "sapply", "mapply", "vapply", "tapply", "by", "aggregate", "outer", "sweep", "kronecker", "eapply")] = "FUN"
IndirectCallFunList[c("Map", "Reduce", "Filter", "Negate", "Find", "Position", "rapply")] = "f"
# Should we include formals, body, body<- ? Yes.
#  Could make it optional in getIndirectCallFunList?
IndirectCallFunList[c("formals", "body", "body<-")] = "fun"


getIndirectCallFunList =
    # access the pre-created list above.
function(..., .els = list(...))    
{
    x = IndirectCallFunList
    if(length(.els)) {
        e = as.character(.els)
        x[ names(e) ] = e
    }
    
    x
}
