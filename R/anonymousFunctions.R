findAnonFunctions =
function(code)
{
    k = findCallsTo(code)
    if(length(k) == 0)
        return(list())
    
    w = sapply(k, function(x) any(sapply(as.list(x), isCallTo, "function")))
    w2 = sapply(k[w], isSimpleAssignTo)
    k2 = k[w][!w2]
    # Can have 2 anonymous functions in the same call and need to keep them separate hence the as.list(x)[]
    # Otherwise get a call() object with the multiple elements.
    lapply(k2, function(x) as.list(x)[sapply(as.list(x), isCallTo, "function")])
}

if(FALSE) {

   quote( lapply(x, function(a) length(x$ab)))



    rf = getRFiles("R")
    p2 = lapply(rf, parse)
    names(p2) = rf
    an = lapply(p2, findAnonFunctions)
    
    d = data.frame(numTerms = sapply(unlist(an),  CodeAnalysis:::numTerms),
                   name = unlist(lapply(an, function(x) rep(names(x), sapply(x, length)))),
                   file = rep(names(an), sapply(an, function(x) length(unlist(x)))))
    d$fun = unlist(an)
    
}


        
