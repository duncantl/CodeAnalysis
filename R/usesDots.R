usesDots =
    # In a function body (node), how does it use ...
    #
function(fun)
{
    c( unlist(lapply(formals(fun), usesDotsI), recursive = FALSE), usesDotsI(body(fun)))
}


# see tests/dots.R
usesDotsI =
function(node)
{
    findCallsTo(node, walker = mkCallWalkerPred(callContainsDots))
}

callContainsDots =
function(call, recursive = FALSE)    
{
        # Probably want to limit to one-level
        # to get foo(list(...)) as a call to foo.
        
    any(sapply(as.list(call),
               function(x) isDots(x) ||
                           if(recursive)
                               isCallTo(x, "list") && length(x) == 2 && isDots(x[[2]])
                           else
                               TRUE))
}

isDots =
function(node)    
  isSymbol(node, "...")


