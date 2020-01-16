
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
          i
      else
          return(lapply(i, function(node) extractS3Class(node)))
  } else
     NULL
}

isS3Assign =
function(n)      
{
  (is(n, "Call") && is(n$fn, "Symbol") && n$fn$value == "class<-") ||
      (is(n, "Call") && is(n$fn, "Symbol") && n$fn$value == "structure" && "class" %in% names(n$args$contents))
}

extractS3Class =
    # node is typically going to be a Call to
    #  class<- "val"
    #  class <- c("val1", "val2")
    #  class <- c("val1", sym)    
    #  class <- foo(...)
    #  structure(..., class = "value")
    #  structure(..., class = c("value", "value2"))
    #  structure(..., class = c("value", variable))
    #  structure(..., class = foo("value", variable))    
    #
function(node)
{
    if(node$fn$value == "structure") {
        #XXX node$args[["class"]]  doesn't work. Fix rstatic.
        i = match("class", names(node$args))
        if(is.na(i)) {
            warning("problems with S3 class assignment")
            return(character())
        }
        tmp = node$args$contents[[i]]
        if(is(tmp, "Call"))
            sapply(tmp$args$contents, function(x) if(is(x, "Character")) x$value else NA)
        else if(is(tmp, "Character"))
            tmp$value
        else
            NA
    } else {
        # call to class<-() so right hand size.
        # Could be  string, c(str, str), foo(...)
        args = node$args[[2]]
        if(is(args, "Call"))
            sapply(args$args$contents, function(x) if(is(x, "Character")) x$value else NA)
        else if(is(args, "Character"))
            args$value
        else if(is(args, "Symbol"))
            NA
        else
            stop("don't yet recognize this class assignment")
    }
}
