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

skipIfFalse =
function(x, w)    
{
    if(isCallTo(x, "if") && isFALSE(x[[2]])) {
        if(length(x) == 4)
            walkCode(x[[4]], w)
        
        return(TRUE)
    }

    FALSE
}

mkCallWalkerPred =
    #
function(pred, ..., skipIfFalse = TRUE)
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

        if(skipIfFalse && skipIfFalse(x, w))
            return(NULL)
        
        isName = is.name(x[[1]])
        if(isSymbol(x[[1]], c("<-", "=")) && is.call(x[[2]]))
            attr(x[[2]], "isLHS") = TRUE
           
           
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
function(funNames, indirect = character(), ...)
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

    mkCallWalkerPred(ok, ...)
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


getIsLHS =
function(x, defaultValue = NA)
{
    if("isLHS" %in% names(attributes(x)))
        return(attr(x, "isLHS"))
    defaultValue
}

isIndirectCall =
    # x is a call.
function(x, indirects, funNames, isFunNamesStrings, isLHS = getLHS(x), envir = globalenv())    
{

    # used in 2 places.
    # Conflicts with the package level matchCall which is slightly different (gets the function from the call)
    # but no problem with the aliasing of the name.
    matchCall = function(fn, x, replace = FALSE) {
        if(replace)
            fn = paste0(as.character(x[[1]]), "<-") 
        fun = get(fn, envir) # XXX
        match.call(fun, x)
    }
    
    if( isSymbol(x[[1]]) && (fn <- as.character(x[[1]])) %in% names(indirects)) {
        argName = indirects[[fn]]

        # If ... in x, then match.call() will raise an error.
        w = sapply(x, function(x) isSymbol(x, "..."))
        if(any(w))
            x = x[!w]

         k = tryCatch(matchCall(fn, x, isTRUE(getIsLHS(x))),
                     error = function(e) {
                         if(is.na(isLHS)) {
                             matchCall(fn, x, TRUE)
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


findCallsRXPred =
function(rx)
{        
    function(x, isName, ...)
    {
        if(isName)
            grepl(rx, as.character(x[[1]]))
        else
            FALSE
    }
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
         walker = if(!missing(rx))
                      mkCallWalkerPred(findCallsRXPred(rx), skipIfFalse = FALSE)
                  else
                      mkCallWalker(funNames, indirect = indirectCallFuns, skipIfFalse = skipIfFalse),
         parse = any(!sapply(funNames, is.name)),
         skipIfFalse = TRUE,
         rx = character())
{
    # try to parse funNames so that we can compare symols not convert symbols to strings and compare
    # But if we can't parse, keep the original string(s).
    if(parse)  
        funNames = lapply(funNames, function(x) tryCatch( parse(text = x)[[1]], error = function(...) x ))


    if(is.character(code)) {
        if(file.exists(code)) {
            if(file.info(code)$isdir)
                code = lapply(getRFiles(code), parse)
            else
                code = parse(code)
        } else
            code = parse(text = code)
    }
    
    
    
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
    ans = walker$ans()
    # Set the name of the function being called as the name of each call, unless it is not a simple function name, but a call.
    names(ans) = sapply(ans, function(x) if(is.symbol(x[[1]])) as.character(x[[1]]) else "")
    ans
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
   mkFunNameList(IndirectCallFunList, .els, rmDups = FALSE)

mkFunNameList =
    #
    #  mkFunNameList(c("A", "B"))
    #  mkFunNameList(c("A", "B"), list())    
    #  mkFunNameList(c("A", "B"), list(a  = "X", b  = "Y", c = "A"))
    #  mkFunNameList(c("A", "B"), "X")
    #
function(var, .els = list(), rmDups = !is.null(names(var)))
{
    x = var
    if(length(.els)) {
        ids = names(.els)
        x = c(x, structure(sapply(.els, as.character), names = ids))
        # Remove duplicates by name, but only if both var and .els have names
        # For the examples above, this won't matter as there are no names
        # and we are merging the contents of the two "vectors".
        if(length(names(var)) > 0 && length(ids) > 0)
            x = x[!duplicated(names(x))]
    }

    # If .els provides a duplicated element but with a name, the
    # element w/o the name remains which is probably not what we want.
    #
    # Some calls to this function have funName = arg and the args are duplicated,
    # but we would care about removing duplicate names
    # But let's do that when merging var and .els as the .els is typically fixed/computed once
    # and so filtered manually. It is the overriding/replacement of those elements by the
    # person merging var and .els that we care about.
    #
    #
    if(rmDups)
        x[!duplicated(x)]
    else
        x
}

