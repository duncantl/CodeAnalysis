library(rstatic)

if(FALSE) {
    source("../explorations/findConcat.R"); e = parse("../explorations/concat.R");
    findConcat(e[1:2])
    findConcat(e[3:4])
    findConcat(e[5:6])    
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

                   # ans = c(ans, val)
                if(node$read$fn$name ==  "c" &&
                    any(sapply(node$read$args, sameNode, node$write))) {
                      nodes <<- c(nodes, node)                     
                }

                 
                if(is(node, "Replacement") && node$write$name %in% names(vars)) {
                     nodes <<- c(nodes, node)
                }
              }
            }
        }
    }
    
    list(process = process, nodes = function() nodes, result = function() list(vars = vars, nodes = nodes))
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
