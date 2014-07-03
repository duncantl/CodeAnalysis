removeConstIf =
function(expr)
{

    if(is.function(expr)) {
        tmp = removeConstIf(b <- body(expr))
        if(class(tmp) == "list" && length(tmp) == 1) tmp = tmp[[1]]
         body(expr) = tmp
        return(expr)
    }

  els = as.list(expr)
  if(class(expr) == "{")
      els = els[-1]
    
  ans = lapply(els, removeConstIf_helper)

  ans = ans[ sapply(ans, length) > 0 ]
    
  if(class(expr) == "{") {
     if(length(ans) == 1)
         return(ans[[1]])
     expr[seq(along = ans) + 1] = ans
     expr = expr[1:(length(ans)+1)]
     expr
  } else if(is.call(expr))
      as.call(ans)
   else
     ans
}

removeConstIf_helper =
function(e) {
      if(is(e, "if")) {
          if(isFalse(e[[2]])) {  
              if(length(e) == 3) # no else
                  return(list())
              else
                 return(removeConstIf(e[[4]]))  # process else
          } else if(isTrue(e[[2]])) # so if(TRUE)
              return( removeConstIf(e[[3]]))
          else {
              e[[3]] = removeConstIf(e[[3]])
              if(length(e) > 3) {
                  e[[4]] = if(class(e[[4]]) == "if")
                              removeConstIf_helper(e[[4]])
                           else
                              removeConstIf(e[[4]])
              }
              e
          }
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
