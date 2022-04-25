setGeneric("callGraph", 
           function(obj, ...)
              standardGeneric("callGraph"))

#
setMethod("callGraph", "character",
          function(obj, asNames = FALSE, ...) {

             if(length(obj) == 1 && grepl("^package:", obj))
                 return(callGraph(getNamespace(gsub("^package:", "", obj)), ...))

              fdefs = findFunctionDefs(obj)
              if(length(fdefs))
                  callGraph(lapply(fdefs, eval), asNames = asNames, ...)
              else {
                  # treat obj as R command as a string. Maybe multiple expressions.
                  expr = parse(text = obj)
                  callGraph(expr)
              }
              
                  
#            if(!asNames) {
#                file.exists(obj)
#            }
         })

tmp =
function(obj, ...) {
    if(length(obj) == 1)
        callGraph(obj[[1]], ...)
    else
       do.call(rbind, lapply(obj, callGraph, ...))
}

setMethod("callGraph", "{", tmp)
setMethod("callGraph", "expression", tmp)


setMethod("callGraph", "environment",
                     # should be Namespace
          function(obj, withinPackage = TRUE, ...) 
          callGraph(as.list(obj), withinPackage, ...))

setMethod("callGraph", "list",
          function(obj, withinPackage = TRUE, recursive = FALSE, ..., .done = NULL) {

              if(length(obj) == 0)
                  return(NULL)

              if(!is.null(.done)) 
                  obj = obj[ !(names(obj) %in% names(.done)) ]
              
              w = sapply(obj, is.function) 

              calls = lapply(obj[w], function(x) unique(getGlobals(x)$functions))
                                              #XXXX check names(calls) is correct here.
              edges = data.frame(caller = rep(names(calls), sapply(calls, length)),
                                 called = unlist(calls), stringsAsFactors = FALSE)

          #?? if any of calls has length 0, add node ???

              # Probably need to refine this to be in "package", not just in the list.
              if(withinPackage)
                  edges = edges[ edges[,2] %in% names(obj) , ]

              class(edges) = c("CallGraphEdges", class(edges))
              rownames(edges) = NULL
              
              if(recursive) {
              #??? should edges[,1] be names(obj)[w] to include fns that don't have any edges, but are in obj??
                  done = addDone(edges[,1], .done)
                  other = setdiff(edges[,2], edges[,1]) 

                  nfuns = lapply(other, function(id) {
                                         x = getAnywhere(id) # , mode = "function"
                                         if(length(x$objs) > 0)
                                             x$objs[[1]]
                                         else
                                             NULL
                                       }) 
                  names(nfuns) = other
                  g2 = callGraph(nfuns, withinPackage = FALSE,
                                 recursive = if(is.numeric(recursive))
                                                 recursive - 1
                                             else
                                                 TRUE,
                                 .done = done)
                  
                  return(rbind(edges, g2))
              }

              edges
          })

setOldClass(c("CallGraphEdges", "data.frame"))
setAs("CallGraphEdges", "igraph", function(from)  igraph::graph_from_edgelist(as.matrix(from)))

plot.CallGraphEdges =
function(x, y, ...)
{
   plot(igraph::graph_from_edgelist(as.matrix(x)), ...)
}



setOldClass(c("FunctionsByFile", "list"))

setMethod("callGraph", "FunctionsByFile",
          function(obj, withinPackage = TRUE, ...) {
              ans = callGraph(structure(unlist(obj), names = unlist(lapply(obj, names)))) # callNextMethod() #  withinPackage, ...)
              fnFileMap = structure(rep(basename(names(obj)), sapply(obj, length)),
                                    names = unlist(lapply(obj, names)))

              ans$callerFile = fnFileMap[ ans[, 1] ]
              ans$calledFile = fnFileMap[ ans[, 2] ]
              ans
          })


if(FALSE) {
setMethod("callGraph", "function",
          function(obj, withinPackage = FALSE, ...) {
              makeCallGraph(obj)  # from CodeDepends.
          })
}

setMethod("callGraph", "function",
          function(obj, withinPackage = FALSE, recursive = FALSE,
                   name = deparse(substitute(obj)),
                   environment = environment(obj), ...) {
              # was makeCallGraph(obj)
              
              calls = getGlobals(obj)$functions
              ans = data.frame(caller = rep(name, length(calls)), called = calls)
              ans
          })


setOldClass(c("call", "language"))
setMethod("callGraph", "call",
          function(obj, withinPackage = FALSE, recursive = TRUE, environment = globalenv(), ...) {

              fnNames = unlist(getCallFunctions(obj))

              fns = lapply(fnNames, get, envir = environment, mode = "function")
              names(fns) = fnNames
              
              callGraph(fns, withinPackage, recursive = recursive)
#              fns = getCallFunctions(fn, environment)
#              ans = callGraph(fns[[1]])

              # get the functions
              # Need to do this recursively, e.g.
              #  foo( x = a(b(c)), y = h(i(j)))
#              fns = c(fns, lapply(obj[-1], function(x) if(is.call(x) && is.name(x[[1]])) get(as.character(x[[1]], environment)) ))
          })


getCallFunctions =
    #
    # For a given call, get the names of the
    # functions being called both.
    # This works recursively on the called function term
    # and each of the arguments

    # !! Assumes a call
    #
    #  unlist(getCallFunctions(quote(  foo(1)(a(b(c)), x(y(z))) )))
    #  [1] "foo" "a"   "b"   "x"   "y"
    #
    #  unlist(getCallFunctions(quote(  x$foo(1)(a(b(c/2)), x(y(z))) )))
    #  [1] "$" "a" "b" "/" "x" "y"
    #
function(x, env = globalenv(), ...)
{
    if(!is.call(x))
        return(character())
    k = x[[1]]
    a = if(is.name(k))
           as.character(k)
        else
            getCallFunctions(k)
    
    c(a, sapply(x[-1], getCallFunctions))
#   else {
#       warning("not handling ", class(x))
#       character()
#   }
    
}


addDone =
function(ids, e = new.env(parent = emptyenv()))
{
    if(is.null(e))
        e = new.env(parent = emptyenv())
    
    sapply(unique(ids), assign, TRUE, e)
    e
}
