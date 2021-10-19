if(FALSE) {
    # Older, slower useful reference implementation.
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
        else if(is(args, "Null"))
            NULL
        else {
            warning("don't yet recognize this class assignment")
            args
        }
    }
}
} # end of if(FALSE)


#################################################

S3Assignments =
function(code, walker = mkS3AssignWalker(...), ...)
{
    walkCode(code, walker)
    walker$ans()
}

mkS3AssignWalker =
function(recursive = TRUE)
{

    defs = list()

    leaf = function(x, w, ...) {
        if(inherits(x, "srcref"))
            return(NULL)
        
        ty = typeof(x)
        if(ty == "pairlist") {
            lapply(x, walkCode, w)
            return(NULL)
        } else if(ty == "closure") {
            walkCode(formals(x), w) 
            walkCode(body(x), w)
        } else if(ty == "call" && is.name(x[[1]]) && as.character(x[[1]]) == "function") {
            browser()
            walkCode(eval(x), w)
        } 

        NULL
    }

    
    call = function(x, w) {
        if(isS3Assign(x))  #  && !is.na((val <- extractS3Class2(x[[3]]))))
            defs[[ length(defs) + 1L ]] <<- extractS3Class(x[[3]])
        
        for (ee in as.list(x))
            if (!missing(ee))
                walkCode(ee, w)
    }

    list(handler = function(x, w) NULL,
         call = call,
         leaf = leaf,
         ans = function() defs)
}

isS3Assign =
function(x)
{
    (is.name(x[[1]]) && ((f <- as.character(x[[1]])) == "<-" ||  f == "=" || f == "<<-") &&
     is.call(x[[2]]) && is.name(x[[2]][[1]]) && (as.character(x[[2]][[1]]) == "class")) ||
         ( is.call(x) && is.name(x[[1]]) && as.character(x[[1]]) == "structure" && "class" %in% names(x))

    #                     (length(x) == 3 && is.call(x[[3]]) && is.name(x[[3]][[1]]) && as.character(x[[3]][[1]]) == "structure" && "class" %in% names(x[[3]])))
}

extractS3Class =
function(x)
{

    val = x # x[[3]]
    
    if(is.call(val) && as.character(val[[1]]) == "structure")
       val = val$class

    if(is.call(val) && is.name(val[[1]]) && as.character(val[[1]]) == "c")
            return(sapply(val[-1], function(x) if(is.character(x)) x else NA))

    if(is.character(val))
        return(val)
    
    NA
}

if(FALSE) {

    tst = function(x) {
        x = x + 1
        class(x) = structure(lapply(x, foo), class = c("a", "x"))
        class(x) = structure(lapply(x, foo), class = "y")
        class(x) = c("1", "2")
        class(x) =  "A"
        class(x) <- structure(lapply(x, foo), class = c("B", "C"))
        class(x) <- structure(lapply(x, foo), class = "D")
        class(x) <- c("E", "F")
        class(x) <-  "G"
        class(x) <-  foo("H")
        class(x) <-  structure(x, class = foo("I"))
        class(x) <-  structure(x, class = c("J", foo("K"))  )
        1+2
        x
    }
    # Missing H and I


    tst2 =
        function(x) {


            lapply(x, function(z) {
                class(z) = "other"
                z
            })
        }

    tst3 =
        function(x) {

            z = list()

            lapply(x, function(z) {
                class(z) <<- "other"
                z
            })
        }    


tmp = lapply(tufuns, S3Assignments2)    
tmp0 = lapply(tufuns, S3Assignments)
    w = sapply(tmp, length)
    w0 = sapply(tmp0, length)

   bad = names(w)[(w != w0)]

    
tmp2 = unlist(tmp, recursive = FALSE)    


tmp00 = unlist(tmp0, recursive = FALSE)


}

