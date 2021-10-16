

setGeneric("callGraph", 
           function(obj, ...)
              standardGeneric("callGraph"))

#
setMethod("callGraph", "character",
          function(obj, asNames = FALSE, ...) {

             if(length(obj) == 1 && grepl("^package:", obj))
                 return(callGraph(getNamespace(gsub("^package:", "", obj)), ...))

             callGraph(lapply(findFunctionDefs(obj), eval), asNames = asNames, ...)
             
#            if(!asNames) {
#                file.exists(obj)
#            }
         })

setMethod("callGraph", "environment",
                     # should be Namespace
          function(obj, withinPackage = TRUE, ...) 
          callGraph(as.list(obj), withinPackage, ...))

setMethod("callGraph", "list",
          function(obj, withinPackage = TRUE, ...) {
              w = sapply(obj, is.function)

              calls = sapply(obj[w], function(x) unique(getGlobals(x)$functions))
                                              #XXXX check names(calls) is correct here.
              edges = data.frame(caller = rep(names(calls), sapply(calls, length)),
                                 called = unlist(calls), stringsAsFactors = FALSE)

              if(withinPackage)
                  edges = edges[ edges[,2] %in% names(obj) , ]

              class(edges) = c("CallGraphEdges", class(edges))
              edges
          })

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
