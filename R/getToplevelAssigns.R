getTopLevelCalls =
function()    
{
    stop("not implemented")
}

getToplevelVariableAssignments =
    #
    # The left hand side has to be a variable name. 
    # So the function ignores x$foo = ... or x[["foo"]] or x[y] = 
    #
function(e)
{    
    w4 = sapply(e, class) %in% c("=", "<-")
    lapply(e[w4], function(x) class(x[[3]]))
}



getToplevelVariableCopies =
    #
    #  Which variables are assigned to another variable
    #    var2 = var
    #  No computations done in the RHS (or LHS)
function()
{
     stop("not implemented")
}


isAssignTo = isSimpleAssignTo =
function(x, var = character())    
    is.call(x) && isSymbol(x[[1]], c("=", "<-")) && is.name(x[[2]]) && (length(var) == 0 || isSymbol(x[[2]], var))


# Could also use
findAssignsTo =
function(code, var = character(), complex = TRUE,
         pred = if(complex) isComplexAssignTo else isAssignTo)
{
    pred2 = function(x, ...) pred(x, var)
    findCallsTo(code, walker = mkCallWalkerPred(pred2))
}



if(FALSE) {
    
setGeneric("findAssignsTo",
function(code, var, index = FALSE, recursive = TRUE, ...)
    standardGeneric("findAssignsTo"))


setMethod("findAssignsTo", "ANY",
          function(code, var, index = FALSE, recursive = TRUE, ...) {
              if(isLiteral(code))
                  return(NULL)

               stop("default method for findAssignTo for ", class(code))
          })


setMethod("findAssignsTo", "function",
          function(code, var, index = FALSE, recursive = TRUE, ...)    
              findAssignsTo(body(code), var, index, recursive))

setMethod("findAssignsTo", "{",
          function(code, var, index = FALSE, recursive = TRUE, ...)    
          {
              ans = sapply(code[-1], findAssignsTo, var, index, recursive)
              if(index)
                  ans + 1
              else
                  ans[sapply(ans, length) > 0]
          })

setMethod("findAssignsTo", "if",
          function(code, var, index = FALSE, recursive = TRUE, ...)    
          {
              if(!recursive)
                  return(NULL)
    
              #XXX fix up if index.
              ans = findAssignsTo(code[[3]], var, index, recursive, ...)
              if(length(code) > 3)
                  ans = c(ans, findAssignsTo(code[[4]], var, index, recursive, ...))
              
              ans
          })

setMethod("findAssignsTo", "list",
function(code, var, index = FALSE, recursive = TRUE, ...)    
{
    w = sapply(code, isAssignTo, var)
    
    if(index)
        which(w)
    else
        lapply(code[w], `[[`, 3)
})


tmp =
function(code, var, index = FALSE, recursive = TRUE, ...)    
{
    if(isAssignTo(code, var))
        code
}
setMethod("findAssignsTo", "<-", tmp)
setMethod("findAssignsTo", "=", tmp)


setMethod("findAssignsTo", "call",
function(code, var, index = FALSE, recursive = TRUE, ...)    
{
    findAssignsTo(as.list(code), var, index, recursive, ...)
})


setMethod("findAssignsTo", "name",          
function(code, var, index = FALSE, recursive = TRUE, ...)    
{
    if(index)
        integer()
    else
        list()
})
} # end if(FALSE) for setGeneric('isAssignTo')






