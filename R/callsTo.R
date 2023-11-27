# find the calls to specified/target functions

isSourceCall =
function(code)
   isCallTo(code, "source")

isCallTo =
function(code, funName, indirect = getIndirectCallFunList(), isLHS = NA)
{
    if(is(code, "ScriptNodeInfo"))
        x = code@code
    
    if(is(code, "R6"))
        is(code, "Call") && is_symbol(code$fn) && code$fn$value %in% funName
    else 
        (is.call(code) || is(code, "call")) &&
         ( isSymbol(code[[1]], funName) ||
              (!isFALSE(indirect) && isIndirectCall(code, indirect, funName, TRUE, isLHS = isLHS)))
}

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
             
            yes = (isName && (as.character(x[[1]]) %in% funNames)) || isNSAccessCallTo(x[[1]], funNames)

            if(yes)
                return(yes)

            if(length(indirect))
                return(isIndirectCall(x, indirect, funNames, isFunNamesStrings))

            FALSE
          }

    mkCallWalkerPred(ok)
}


# Functions to check for pkg::foo or pkg:::foo
#  and to check if that name is in funNames, either the full name pkg::foo or foo.
isNamespaceAccess =
function(x)
  is.call(x) && isSymbol(x[[1]], c("::", ":::")) 


isNSAccessCallTo =
function(x, funNames)
{
  is.call(x) && isNamespaceAccess(x) &&
      (deparse(x) %in% funNames || (is.name(x[[3]]) && as.character(x[[3]]) %in% funNames))
}


isIndirectCall =
    # x is a call.
function(x, indirects, funNames, isFunNamesStrings, isLHS = NA, envir = globalenv())    
{
    if( isSymbol(x[[1]]) && (fn <- as.character(x[[1]])) %in% names(indirects)) {
        argName = indirects[[fn]]
        fun = get(fn) # XXX
        # If ... in x, then match.call() will raise an error.
        w = sapply(x, function(x) isSymbol(x, "..."))
        if(any(w))
            x = x[!w]
        k = tryCatch(match.call(fun, x),
                     error = function(e) {
                         if(is.na(isLHS) || isLHS) {
                             id = paste0(as.character(x[[1]]), "<-") 
                             x[[1]] = as.name( id )
                             match.call(get(id), x)
                         } else
                             stop(e)
                     })
        
        if(!(argName %in% names(k)))
            return(FALSE)
        
        arg = k[[argName]]
        if(!(is.character(arg) || isSymbol(arg) || isNamespaceAccess(arg)))
            return(FALSE)  

        if(isNamespaceAccess(arg))
            return(isNSAccessCallTo(arg, funNames))
        
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
         indirectCallFuns = getIndirectCallFunList(),
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
