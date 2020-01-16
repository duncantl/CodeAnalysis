

setGeneric("callGraph", 
           function(obj, ...)
              standardGeneric("callGraph"))

#
setMethod("callGraph", "character",
          function(obj, asNames = FALSE, ...) {

             if(length(obj) == 1 && grepl("^package:", obj))
                 return(callGraph(getNamespace(gsub("^package:", "", obj)), ...))

#            if(!asNames) {
#                file.exists(obj)
#            }
         })

setMethod("callGraph", "environment",
                     # should be Namespace
          function(obj, withinPackage = TRUE, ...) {
              obj = as.list(obj)
              w = sapply(obj, is.function)

              calls = sapply(obj[w], function(x) getGlobals(x)$functions)
                                              #XXXX check names(calls) is correct here.
              edges = data.frame(caller = rep(names(calls), sapply(calls, length)),
                                 called = unlist(calls), stringsAsFactors = FALSE)

              if(withinPackage)
                  edges = edges[ edges[,2] %in% names(obj) , ]

              class(edges) = c("CallGraphEdges", class(edges))
              edges
          })
