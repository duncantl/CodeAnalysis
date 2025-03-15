#
#

if(FALSE) {
    findOptionsUses(quote(options()["abc"]))
    findOptionsUses(quote(options()[c("abc", "def")]))
    findOptionsUses(quote(options()[c("abc", x)]))        
    findOptionsUses(quote(options()[x]))
    findOptionsUses(quote(options()[["abc"]]))    
    findOptionsUses(quote(options()$abc))
    findOptionsUses(quote(options()$"abc"))        
}


findOptionsUses =
function(call, isName)    
{

    w = isCallTo(call, "options") && length(call) == 2 && is.character(call[[2]])
    w = w | isCallTo(call, "getOption")
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
#  k = findCallsTo(code, "getOption")
#  w = sapply(k, function(x) length(x) > 1 && is.character(x[[2]]))
#  optNames1 = sapply(k[w], `[[`, 2)

    k2 = findCallsTo(code, walker = mkCallWalkerPred(findOptionsUses))
    sapply(k2, getUsedOptionName)
#    c(optNames1, optNames2)
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


if(FALSE) {
    k = findCallsTo(code, "getOption")
    w = sapply(k, function(x) length(x) > 1 && is.character(x[[2]]))
    optNames1 = sapply(k[w], `[[`, 2)    

    nargs = sapply(k, length) - 1
    w = (sapply(k, isCallTo, "getOption") & nargs > 0) | (sapply(k, isCallTo, "options") & nargs > 0) 
    k = k[w]
    
    if(asNodes)
        k
    else
        sapply(k, getUsedOptionName)
}


if(FALSE) #<<<<<<<<<<
getUsedOptionName =
function(node)
{
    if(node$fn$value %in% c("options", "getOption")) {
        els = node$args$contents
    } else if(node$fn$name == "$") {  # must be  options()$... or [[...]] or [
       return(node$args[[2]]$name)
    } else # [[ or [
        els = node$args[ - 1 ]


    w = sapply(els, is, "Character")
    sapply(els[w], function(x) x$value)    
}
