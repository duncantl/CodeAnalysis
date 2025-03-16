countNestedFunctionLevel =
    #
    # Given an rstatic Function object, this walks up the AST 
    #
    #
function(f, count = 0)
{
    f2 = asFunction(f$parent)
    if(!is.null(f2))
        return(countNestedFunctionLevel(f2, count + 1))

    count

}


# Instead of propagating the constants before analysis, for now we will
# find a Symbol and walk back through the script to see if we have a literal value
# This is for an rstatic AST object.

findLiteralValue =
function(sym)
{
        # Assume an argument in an ArgumentList so get to the call.
   call = sym$parent$parent

   idx = where_is(asToplevelExpr(call))
   
   script = asScript(call)
   before = script$contents[rev(seq_len(idx - 1))]
   lit = sapply(before, function(x) is(x, "Assignment") && x$write == sym && is(x$read, "Literal"))
   if(any(lit))
       as_language(before[[ which(lit)[1] ]]$read)
   else
       sym
}

asScript =
function(x)
{
    while(!is.null(x$parent))
        x = x$parent
    x
}

asToplevelExpr =
function(x)
{
    if(is.null(x$parent))
        return(x)
    
    while(!is.null(x$parent$parent))
        x = x$parent
    x
}

asFunction =
function(x)
{
    while(!is.null(x)) {
        if(is(x, "Function"))
            return(x)
        x = x$parent
    }
    
    NULL
}



###########

#############
# See S3Assigns.R

findS3ClassDefs =
function(x, ...)
   UseMethod("findS3ClassDefs")

findS3ClassDefs.character =
function(x, ...)
    findS3ClassDefs(parse(x), ...) # deal with vector, directory.

findS3ClassDefs.expression = findS3ClassDefs.function =
function(x, ...)
{
    w = mkCallWalkerPred(isS3ClassSetNode)
    k = findCallsTo(x, walker = w)
    # Now have to post process the nodes.

    # extractS3Class wants the RHS of the class() = ..
    # not the full assignment call. So this would need to be
    # fixed if we want to use this function. But it is not exported
    # or called.
    browser()
    lapply(k, extractS3Class)
}


isS3ClassSetNode =
function(x, isName, ...)
{
    if(isCallTo(x, "structure") && "class" %in% names(x))
        return(TRUE)
    

    if(!isComplexAssignTo(x))
        return(FALSE)

    lhs = x[[2]]
    
    if(isCallTo(lhs, "class"))
        return(TRUE)

    isCallTo(lhs, "attr") && length(lhs) >= 2 && is.character(x[[2]]) && x[[2]] == "class"
}



#########
#freeVariables.R

dropNotRunCode.R6 =
function(x, ...)    
{
    nodes = find_nodes(x, isIfFalse)
    if(length(nodes)) 
           lapply(nodes, function(x) children(x$parent) = children(x$parent)[ - where_is(x) ])

    x
}
