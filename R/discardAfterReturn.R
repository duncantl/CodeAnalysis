removeAfterReturn =
    #
    # discard any expressions after a top-level return() command.
    # Programmers leave code after return() (I do!) and we should not compile this.
    #
    #
    #  We should make this recursive, i.e. process the bodies of for/while loops
    #
function(fun)
{
  b = body(fun)
  if(class(b) != "{")
      return(fun)

  e = as.list(b)[-1]
  i = sapply(e, isReturn)
  end = which(i)[1]
  if(end < length(e)) {
     body(fun) =  b[1:(end + 1)]
  }
  
  fun
}

isReturn =
function(e)
 is.call(e) && length(e[[1]]) == 1 && as.character(e[[1]]) == "return"
