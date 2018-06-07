library(rstatic)

if(FALSE) {

    source("../explorations/concat/findConcat.R"); e = parse("../explorations/concat/concat.R");

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

    a$set_fn(Symbol$new("integer"))

    findConcat(e[5:6])    

    found = findConcat(code)$nodes[[1]]

    found$parent$parent

    preallocate1(e[1:2])    

}


preallocate1 =
#
# This only works for that looks EXACTLY like case 1:
# 
# ans = c()
# for(xi in x) {
#    ans = c(ans, f(xi))
# }
# 
# Transform to version that does preallocation:
#
# ans = rep(NA, length(x))
# for(i in seq_along(x)) {
#    ans[i] = f(x[i])
# }
function(code)
{

    ast = to_ast(code)
    # Not worrying about nested loops for the moment
    found = findConcat(code, ast = ast)

    # Seems unnecessary to loop here, since probably only works for one
    # found variable anyways.
    for(i in seq_along(found$vars)){

        init = found$vars[[i]]$parent
        concat = found$nodes[[1]]
        loop = parentForLoop(concat)

        # Clark: Doing this because it's easier and I'm more likely to get
        # it right vs calling the $new() as Duncan uses elsewhere.
        # What's the difference if I call the $copy() method?

        # Nick: Yeah, code generation is a little too verbose right now. I
        # haven't used rstatic to generate much code, and this is definitely a
        # rough edge that should be fixed.
        #
        # The $new() method is a static constructor for R6 classes. Every
        # R6 class has $new().
        # 
        # The $copy() method makes a deep copy of an existing node and its
        # descendants. Normally with R6 you would use $clone(deep = TRUE) to
        # make a deep copy, but if you use that on an ASTNode, the node's
        # children will still have the original as their parent.

        # ans = rep(NA, length(x))
        init$read = quote_ast(rep(NA, length(REPLACE_ME)))

        # Nick: Use $copy() on loop$iter here to so that you don't get multiple
        # references to the same object. You generally need to $copy() unless
        # the object's old location is no longer part of the tree.
        #
        # I'm not sure when that's the case in your code, so I've probably
        # added $copy() in more places than you really need.
        init$read$args[[2]]$args = list(loop$iter$copy())

        # TODO: If there's any other code in the body of the loop then this
        # will probably break it.

        # for(i in seq_along(x)) {

        # Nick: Same thing as above with $copy().
        loop$iter = Call$new("seq_along", args = list(loop$iter$copy()))

        # Clark: I would like a way to change the variables with basename i
        # in the following subtree to the loop iteration variable. How to do
        # this? Without doing everything in rstatic's intro vignette.

        # Nick: There's no built-in function to rename variables (yet).
        #
        # You can use node_apply() to quickly apply a transformation function
        # to nodes of interest without having to write methods for the entire
        # traversal. Example (working with rstatic_0.0.0.9031):
        #
        # rename_var = function(node) UseMethod("rename_var")
        # rename_var.ASTNode = function(node) node
        #
        # # Override what happens to Symbols.
        # rename_var.Symbol = function(node) {
        #   if (node$basename == "i")
        #     node$basename = "j"
        #
        #   node
        # }
        #
        # node_apply(YOUR_AST, rename_var)

        original = concat$copy()

        fcall = concat$read$args[[2]]

        # out[j] = g(y[j])
        concat$write = quote_ast(out[j])
        concat$write$args = list(init$write$copy(), loop$ivar$copy())

        rhs = concat$read = quote_ast(g(y[j]))
        rhs$fn = fcall$fn$copy()
        rhs$args[[1]]$args[[1]] = init$write$copy()
        rhs$args[[1]]$args[[2]] = loop$ivar$copy()
    }
    ast
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
                if(!is.null(parentForLoop(node))) {
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


parentForLoop =
#
# Return the node corresponding to the first found containing for loop, and
# NULL if not inside a for loop
function(node)
{
    while(!is.null(node)) {
        if(is(node, "For"))
           return(node)
        node = node$parent
    }
    NULL
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
