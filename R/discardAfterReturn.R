removeAfterReturn =
    #
    # discard any expressions after a top-level return() command.
    # Programmers leave code after return() (I do!) and we should not compile this.
    #
    #
    #  We should make this recursive, i.e. process the bodies of for/while loops
    #
function(fun, recurse = TRUE)
{

  isFunc = is.function(fun)
  if(isFunc) {
      b = body(fun)
  } else
      b = fun
  
#  if(class(b) != "{")
#      return(fun)  
  
  e = as.list(b)[-1]
  i = sapply(e, isReturn)
#  if(!any(i)) 
#      return(fun)
  if(any(i)) { 
     end = which(i)[1]
     if(end < length(e)) 
        b =  b[1:(end + 1)]
  }

  if(recurse && !is.name(b) && !is.atomic(b)) {
      b[] = lapply(b, removeAfterReturn, TRUE)
  }
  
  if(isFunc) {
      body(fun) = b
      fun
  } else
      b
}

isReturn =
function(e)
 is.call(e) && length(e[[1]]) == 1 && as.character(e[[1]]) == "return"
