findLoopConcat =
    #
    # Detect the idiom  where we loop over a vector and concatenate individual results
    # to another vector, e.g.,
    #   x = c()
    #   for(i in z) 
    #      x = c(x, f(i))
    #
    # This returns
    #  e = quote({ x = c(); for(i in z)  x = c(x, f(i)) })
    # findLoopConcat(e)
    #
    #
    # Note that expression( {...}) doesn't behave.
    # It just returns. This is because as.list doesn't behave itself.
    # Fixed now.
    #
function(expr, possibleVars = character())
{
   isFunc = is.function(expr)
   if(isFunc) {
       fun = expr
       expr = body(expr)
   } 

      # if we have an expression with one block of expressions, as.list() won't get its components, so get the
      # "{" object.
  if(is.expression(expr) && length(expr) == 1 && class(expr[[1]]) == "{")
      expr = expr[[1]]
   
   for(e in as.list(expr)) {
       if(class(e) == "{")
           next
        else if(class(e) == "for") {
            b = if(class(e[[4]]) != "{")  list(e[[4]]) else e[[4]]
            ans = findLoopConcat(b, possibleVars)
            if(is.character(ans))
                return(ans)
       } else if(is.call(e)) {
           funName = as.character(e[[1]])
           if(length(funName) == 1 && funName %in% c("=", "<-")) {
               rhs = e[[3]]

                 # See if we have the start, i.e. x = c()
               if(length(e[[2]]) == 1 && is.name(e[[2]]) &&
                  length(rhs) == 1 && as.character(rhs[[1]]) %in% c("c", "integer", "logical", "numeric", "list"))
                   possibleVars = c(possibleVars, as.character(e[[2]]))
               else if(length(possibleVars) && length(e[[2]]) == 1 && is.name(e[[2]]) && (lhs <- as.character(e[[2]])) %in% possibleVars) { # e.g. x =
                   rhs = e[[3]]
                   if(length(rhs) == 3 && is.name(rhs[[1]]) && as.character(rhs[[1]]) == "c" && is.name(rhs[[2]])  && as.character(rhs[[2]]) == lhs)
                       return(lhs)
               }
           }
       }
   }
   return(character())
}


rewriteConcatLoop =
    #
    # Not implemented.
    #
function(expr)
{
   expr
}
