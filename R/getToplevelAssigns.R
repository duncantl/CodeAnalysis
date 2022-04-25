getTopLevelCalls =
function()    
{

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
}



setGeneric("findAssignsTo",
function(code, var, index = FALSE, recursive = TRUE, ...)
    standardGeneric("findAssignsTo"))


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

isAssignTo =
function(x, var)    
    is.call(x) && is.name(x[[1]]) && as.character(x[[1]]) %in% c("=", "<-") && is.name(x[[2]]) && as.character(x[[2]]) == var


