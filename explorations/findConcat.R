library(rstatic)

if(FALSE) {

    source("../explorations/findConcat.R"); e = parse("../explorations/concat.R");

    findConcat(e[1:2])
    findConcat(e[3:4])
    findConcat(e[5:6])


    # When rewriting the code
    found = findConcat(e[1:2])

    # Something like
    # This is what we want in the ans = c() to become
    #  ans = integer(length(x))
    #
    # This gives the length(x)
    len = Call$new("length", args = list(Symbol$new("x")))

    # So then we have the LHS. So up one step to the parent and
    # then get the RHS - the read.
    # We'll insert into that.
    a = z$vars[[1]]$parent$read
    a$args[[1]] = len
    #
    # And change c() to be let's say an integer().  Depends on the return type of i
    # Clark: 

    a$set_fn(Symbol$new("integer"))

    findConcat(e[5:6])    

    found = findConcat(code)$nodes[[1]]

    found$parent$parent

}


findConcat =
    #
    # Start by finding var = c(var, value)
    #
function(code, collector = concatCollector(), ast = to_ast(code))    
{
    astTraverse(ast, collector$process)
    collector$result()
}


concatCollector =
function()
{
    nodes = list()
    vars = character()
    process = function(node) {

    if(is(node, "Assign")) {

        if(is(node$write, "Symbol")) {
            if( is(node$read, "Call")) {
                if(node$read$fn$name %in% c("c", "integer", "numeric", "logical")
                   && (length(node$read$args)  == 0 || (length(node$read$args) == 1 && node$read$args == 0))) {
                    vars <<- c(vars, node$write)
                     names(vars)[length(vars)] <<- node$write$name
                }

                 # Check if in the body of a for loop.
                if(inForLoop(node)) {
                        # ans = c(ans, val)
                    if(node$read$fn$name ==  "c" &&
                       any(sapply(node$read$args, sameNode, node$write))) 
                        nodes <<- c(nodes, node)                     

                        # ans[i] = val
                    if(is(node, "Replacement") && node$write$name %in% names(vars)) 
                        nodes <<- c(nodes, node)
                }
              }
            }
        }
    }
    
    list(process = process, nodes = function() nodes, result = function() list(vars = vars, nodes = nodes))
}


inForLoop =
function(node)
{
    while(!is.null(node)) {
        if(is(node, "For"))
           return(TRUE)
        node = node$parent
    }
    FALSE   
}

sameNode =
    # Should go into rstatic package.
function(a, b, exclude = c("ssa_number", "parent"))
{
    if(!all(class(a)[1] == class(b)[1]))
        return(FALSE)
    
  members = setdiff(ls(a), exclude)
  is_method = vapply(members, function(f) is.function(a[[f]]), TRUE)    

  fields = members[!is_method]
  for(i in fields) {
      if(!identical(a[[i]], b[[i]]))
          return(FALSE)
  }

  TRUE
}    
