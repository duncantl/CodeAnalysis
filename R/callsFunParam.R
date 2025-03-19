
# I think I already implemented this. Look in the R files.
#  findCallsParam ???
# # !!! Look in freeVariables.R  - usedAsFunction and isParamUsedAsFun
#
# The idea here is to find functions which take a parameter
# and then call/invoke that parameter as a function, e.g. lapply, mapply, curve, optim, do.call, outer
# RCIndex::parseTU() and visitTU() (although that hits C code). Or Rllvm.
#
#
#
#


findCallsParam =
    #
    # How is this related to getCallParam()? (freeVariables.R)
    #
    # Find any direct calls to a parameter, e.g.
    #   optim() may call its fn and gr
    #
    # ? Doesn't find indirect calls such as function(x, f) sapply(x, f).
    # Get those with getGlobals().
    # Actually, findCallsTo does find use in lapply()
    #
    # Doesn't detect calls from C code of course. See the RCIndex package 
    # and NativeCodeAnalysis package for that..
    #
    #
    #
function(fun, asCalls = FALSE, indirect = getIndirectCallFunList())
{
    params = names(formals(fun))

    if(length(params) == 0)
        return(if(asCalls) list() else character())

    v = findCallsTo(fun, params)

    v = v[ sapply(v, function(x) is.null(attr(x, "isLHS"))) ]

    if(asCalls)
        v
    else {
        if(length(v) == 0)
            return(character())
        ans = sapply(v, getCalledParam, params, indirect)
        unique(ans)
    }
}

getCalledParam =
    # should we use deparse rather than as.character(). Shouldn't matter as direct call to parameter name.
    # Fixed to also handle indirect calls to parameter but return that parameter name as the parameter
    # being called.    
function(x, paramNames, indirect = getIndirectCallFunList())
{
    if(isCallTo(x, paramNames, indirect = character()))
        return( as.character(x[[1]]) )

    if(!isSymbol(x[[1]]))
        #XXX
        return(NA)

    fn = as.character(x[[1]])

    # index into the call, so 1 more than the argument.
    # Should use match.call() to order the arguments in the call.
    # Can use getIndirectCallFunList() to get the argument by name or position.
    fn = as.character(x[[1]])
    k = match.call(get(fn), x)
    i = match(indirect[fn], names(k))

    if(is.na(i)) {

        # not include formals(), body(), match.fun(), but could as
        # this indicates the param is expected to be a function.
        i = switch(fn,
                   Filter = ,
                   Find = ,
                   Map =,
                   Negate =,
                   Position =,
                   Reduce =,
                   do.call = 2,
                   sapply =,
                   lapply =,
                   vapply = ,
                   eapply = ,
                   rapply = 3,               
                   by = ,
                   tapply =,
                   outer = ,
                   apply = ,
                   aggregate = 4, # but may be a method.
                   mapply = 2,
                   sweep = 5,
                   NA)
    }
    
    if(is.na(i))
        return(NA)

    as.character(x[[ i ]] )
}

