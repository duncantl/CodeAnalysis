removeConstIf =
function(expr, debug = FALSE)
{
    
    if(is.function(expr)) {
        body(expr) = tmp = removeConstIf(b <- body(expr))
        return(expr)
    }

if(debug)browser()        
if(is.call(expr) && as.character(expr[[1]]) == "b") browser()    

  els = as.list(expr)
  if(class(expr) == "{")
      els = els[-1]
    
  ans = lapply(els, function(e) {
      if(is(e, "if")) {
browser()          
          if(isFalse(e[[2]])) {
              if(length(e) == 3)
                  return(list())
              else
                 return(removeConstIf(e[[4]]))
          } else if(isTrue(e[[2]]))
              return( removeConstIf(e[[3]]))
      } else if(is(e, "for")) {
            # Should do e[[3]] in case it has an if()
          e[[4]] = removeConstIf(e[[4]], debug = TRUE)
          return(e)
      } else if(is.call(e) && as.character(e[[1]]) == "function") { # have to chek e[[1]] is is.name() and not a call like a$foo.
          e[[length(e)]] = removeConstIf(e[[length(e)]])
          e
      } else if(is.call(e))
          as.call(removeConstIf(e))
      else
          e
  })


  ans = ans[ sapply(ans, length) > 0 ]
    

  if(class(expr) == "{") {
     expr[seq(along = ans) + 1] = ans
     expr = expr[1:(length(ans)+1)]
     return( expr)
  } else
      return(ans)


    
  if(class(expr) == "{")
      ans = structure(c(bquote(`{`), ans), class = class(expr))
     #class(ans) = class(expr)
  ans
}


isFalse =
function(cond, target = FALSE)
{
  (is.logical(cond) && cond == target) ||
       # a compound expression of the form FALSE && expr
      (!is.name(cond) && as.character(cond[[1]]) == "&&" && any(sapply(cond[-1], isFalse)))
}

isTrue =
function(cond, target = TRUE)
{
  (is.logical(cond) && cond == target) ||
       # a compound expression of the form FALSE && expr
       (!is.name(cond) &&  as.character(cond[[1]]) == "||" && any(sapply(cond[-1], isTrue)))
}
