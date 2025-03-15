#
#

if(FALSE) {
    # See tests/options.R
    findOptionsUses(quote(options()["abc"]))
    findOptionsUses(quote(options()[c("abc", "def")]))
    findOptionsUses(quote(options()[c("abc", x)]))        
    findOptionsUses(quote(options()[x]))
    findOptionsUses(quote(options()[["abc"]]))    
    findOptionsUses(quote(options()$abc))
    findOptionsUses(quote(options()$"abc"))        
}


findOptionsUses =
    #
    # predicate function for a call to determine whether it is
    # getOption('name')
    # options('name')
    # options()[ literal ]
    # options()[ c(literal, literal2, ...) ]
    # options()[[ literal ]]
    # options$literal
    #
function(call, isName)    
{
    # should do match.call() to ensure the arguments are in the correct order.
    w = isCallTo(call, "getOption") && length(call) >= 2 && is.character(call[[2]])
    if(w)
        return(TRUE)
    
    w = isCallTo(call, "options") && length(call) == 2 &&
           (is.null(names( k <- match.call(options, call))) || "x" %in% names(k)) &&
              is.character(k[[2]])
    
    if(w)
        return(TRUE)
    
    if(!isCallTo(call, c("[[", "$", "[")))
        return(FALSE)

    op = as.character(call[[1]])
    isCallTo(call[[2]], "options") &&
          ( isLiteral(call[[3]]) || (op == '$' && is.name(call[[3]] )))
}


findUsedOptions =
function(code, asNodes = FALSE)
{
    k = findCallsTo(code, walker = mkCallWalkerPred(findOptionsUses))
    sapply(k, getUsedOptionName)
}

getUsedOptionName =
function(x)
{
    if(isCallTo(x, c("options", "getOption")))
        return(x[[2]])

    # Otherwise, call of form options()[[ ]] or $ or [ with literals in the subsetting.
    ans = x[[3]]
    if(is.call(ans)) 
        return( as.character(ans[-1]) )

    ans
}



