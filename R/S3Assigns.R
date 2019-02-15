
S3Assignments =
    #
    #
    #
function(fun, asNode = FALSE)
{
  fun = to_ast(fun)
  i = find_nodes(fun, isS3Assign)
  if(length(i)) {
      if(asNode)
          return(lapply(i, function(idx) fun[[idx]]))
      else
          return(lapply(i, function(idx) extractS3Class(fun[[idx]])))
  } else
     NULL
}

isS3Assign =
function(n)      
{
  is(n, "Call") && is(n$fn, "Symbol") && n$fn$name == "class<-"
}

extractS3Class =
    # node is typically going to be a Call to class<-
    # and we will want the second argument and to get the literals
function(node)
{

    i =  find_nodes(node$args[[2]], is, "Character")
    if(length(i))
        unlist(lapply(i, function(idx) node$args[[2]][[idx]]$value))
    else
        character()
}
