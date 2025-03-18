
# Here we check for unnecessary assigns of the form
#  ans = foo()
#  ans
# at the end of a block of expressions

# We can extend this to
# return(ans)
# invisible(ans)
# and
# invisible(return(ans))
#
# see assignReturn_eg.R
#
#

setGeneric("isAssignReturn",
           function(code, ...)
             standardGeneric("isAssignReturn")
           )

setMethod("isAssignReturn", "function",
          function(code, ...) {
              isAssignReturn(body(code), ...)
          })

setMethod("isAssignReturn", "{",
          function(code, ...) {

              n = length(code)
              if(n == 2)
                  return(FALSE)

              end = code[c(n-1, n)]
              retName = getReturnVarName(end[[2]])
              if(is.null(retName))
                  return(FALSE)
              class(end[[1]]) %in% c("<-", "=") &&
                   is.name(end[[1]][[2]]) && identical(retName, end[[1]][[2]])

              
# end = to_ast(code)              
#              is(end[[1]], "Assign") && end[[1]]$write
      })


getReturnVarName =
function(e)
{
    if(is.name(e))
        return(e)
    
    if(isLiteral(e))
        return(NULL)

    if(is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) %in% c("return", "invisible"))
        return(getReturnVarName(e[[2]]))

    return(NULL)
}


