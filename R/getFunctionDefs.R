####################
#
#  Does this work recursively if passed a function object.
#  Yes, if recursive = TRUE.
#

setGeneric("getFunctionDefs",
    # Read a file, an environment, a parse tree and find the top-level
    # function definitions
    # How does this compare/differ from findFunctionDefs()
function(x, ...)
 standardGeneric("getFunctionDefs"))

setMethod("getFunctionDefs", "character",
    #
    #XXX vectorize in x.  See/use generalCharacterMethod ?
function(x, unlist = TRUE, recursive = FALSE, parse = TRUE, warn = TRUE, ...)
{
    if(!parse)
        return(NULL)
    
    if(file.exists(x) && file.info(x)$isdir[1]) {

            files = getRFiles(x)
            tmp = lapply(files, getFunctionDefs, recursive = recursive, ...)

            if(!recursive) { # !("recursive" %in% names(list(...))) && list(...)$recursive) {
                tt = table(unlist(lapply(tmp, names)))

                # Could allow warn to be either TRUE/FALSE or a character vector and then only warn if the duplicate names are in this vector.
                if(warn && any(tt > 1))
                    warning("multiple definitions for functions ", paste(names(tt)[tt > 1], collapse = ", "))
            }

            return(if(unlist) {
                     ans = structure(unlist(tmp), names = unlist(lapply(tmp, names)))
                     if(recursive && length(ans)) {
                         attr(ans, "nestLevel") = unlist(lapply(tmp, attr, "nestLevel"), recursive = FALSE)
                         attr(ans, "nestInfo") = unlist(lapply(tmp, attr, "nestInfo"), recursive = FALSE)
                     }
                     ans
                   } else
                     structure(tmp, names = files, class = "FunctionsByFile"))
    }

    e =  if(file.exists(x))
            parse(x)
        else
            parse(text = x)

  getFunctionDefs(e, recursive = recursive, parse = FALSE, ...)
})


setMethod("getFunctionDefs", "environment",
function(x, ...)
    getFunctionDefs( as.list(x, all = TRUE), ...))

setMethod("getFunctionDefs", "list",
function(x, recursive = FALSE, ...)
{
    ans = x[ sapply(x, is.function) ]
    if(recursive) {
        tmp = lapply(ans, getAllFunctionDefs, recursive = TRUE, ...)
                #  put the names on the result. 
        tmp = mapply(function(x, nm) { names(x)[1] = nm; x}, tmp, names(tmp), SIMPLIFY = FALSE)
        tmp2 = unlist(tmp, recursive = FALSE)
        names(tmp2) = unlist(lapply(tmp, names))
        if(!is.null(tmp2)) {
            attr(tmp2, "nestLevel") = unlist(lapply(tmp, attr, "nestLevel"))
            attr(tmp2, "nestInfo") = lapply(tmp, attr, "nestInfo")
        }
        tmp2
    } else
        ans
})


setMethod("getFunctionDefs", "expression",
          #XXXX  This doesn't seem to directly call any getFunctionsDef and hence its methods for the language elements!
          # Do the other functions call getFunctionDefs?, i.e., does getAllFunctionDefs call getFunctionDefs. No.
function(x, envir = new.env(parent = globalenv()),
         toSymbol = TRUE, recursive = FALSE,
         funsReturningFuns = FunctionsReturningFunctions,
         ...)
{
    if(length(x) == 0)
        return(list())

    if(recursive)
        return(getAllFunctionDefs(x, recursive = TRUE, envir = envir, ...))
    
    w = sapply(x, isFunAssign, toSymbol = toSymbol)

    ans = if(toSymbol) {
              lapply(x[w], eval, envir)
              as.list(envir, all.names = TRUE)
          } else {
              defs = x[w]
              funs = lapply(x[w], function(e) eval(e[[3]], envir))
              names(funs) = sapply(x[w], function(e)
                                            if(is.name(e[[2]]))
                                                as.character(e[[2]])
                                            else
                                                paste(deparse(e[[2]]), collapse = ""))
              funs
          }

    # Now look for top-level assignments that define functions but indirectly via calls to functions
    # that return functions.
    #
    # This will also pick up x$el = function(...)
    # So filter them.
    w2 = sapply(x[!w], isFunctionDef, funsReturningFuns)
    if(any(w2)) {
        els = x[!w][w2]
        els = els[ sapply(els, function(x) is.name(x[[2]])) ]
        if(length(els)) {
            ids = sapply(els, function(x) as.character(x[[2]]))
            ans[ids] = lapply(els, `[[`, 3)
        }
    }

    ans
})


tmp = function(x, parse = FALSE, recursive = FALSE, envir = globalenv(), ...)
{
#XXX Get the names on the elements in this and the .function method    
    if(isFunAssign(x)) {
        ans = getFunctionDefs(x[[3]], parse = parse, recursive = recursive, envir = envir, ...)
        # If we have a single function object, we can't put the name on that.
        # So can put it in a list.
        if(!is.list(ans))
            ans = list(ans)
        names(ans)[1] = as.character(x[[2]])
        ans
    } else if(isFunctionDef(x)) {
        # Sets the names on a object that may have more than one element to just one name
        # so get c(varName, "", "")
        structure(x[[3]], names = as.character(x[[2]]))
    } else if(is.name(x[[1]]) && as.character(x[[1]]) == "function") {
        #
        # Decided that this should return the top-level function itself
        # Different from method for function.
        tmp = eval(x, envir)
        if(recursive)
            unlist(c(tmp, getFunctionDefs(eval(x), parse = parse, recursive = TRUE, envir = envir, ...)))
        else
            tmp
    } else
        unlist( lapply(as.list(x), getFunctionDefs, parse = parse, recursive = recursive, envir = envir, ...) )
}
setMethod("getFunctionDefs", "call", tmp)
#XXX Need to setOldClass("<-")
#??? Why set this for <- but not for = which is set later with a different function?
setMethod("getFunctionDefs", "<-", tmp)


setMethod("getFunctionDefs", "function",
          #XXXX implement recursive = TRUE
          # This seems to be the only operation.
          # If not recursive, should simply return NULL as this is the function definition.
function(x, parse = FALSE, recursive = FALSE, ...)
   #XXXX if don't have parse here, problems with 2 argument named parse in subsequent recursive calls
{
    p = lapply(formals(x), getFunctionDefs, parse = parse, recursive = recursive, ...)
    unlist(c(p[sapply(p, length) > 0],
             getFunctionDefs(body(x), parse = parse, recursive = recursive, ...)))
})


tmp = function(x, ...)
        list()
setMethod("getFunctionDefs", "complex", tmp)
setMethod("getFunctionDefs", "integer", tmp)
setMethod("getFunctionDefs", "logical", tmp)
setMethod("getFunctionDefs", "numeric", tmp)
setMethod("getFunctionDefs", "name", tmp)
setMethod("getFunctionDefs", "NULL", tmp)


tmp =     
function(x, ...)
{
    drop = switch(class(x), "for" = c(1,2), 1)
    ans = lapply(x[- drop], getFunctionDefs, ...)
    unlist(ans[sapply(ans, length) > 0])
}

setMethod("getFunctionDefs", "{", tmp)
setOldClass("=")
setMethod("getFunctionDefs", "=", tmp)
setMethod("getFunctionDefs", "if", tmp)
setMethod("getFunctionDefs", "while", tmp)
setMethod("getFunctionDefs", "for", tmp)
setMethod("getFunctionDefs", "(", tmp)



if(FALSE) {
    getFunctionDefs.default =
        function(x, ...)
            getFunctionDefs( as.list(x), ...)
}

if(FALSE) {
    # Think about adding these
getFunctionDefs.default =
function(x, ...)
    lapply(as.list(x), getFunctionDefs, ...)



getFunctionDefs.call =   # see above.
function(x, ...)
   lapply(x, getFunctionDefs, ...)
}

#################################
