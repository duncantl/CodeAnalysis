####################

setGeneric("getFunctionDefs",
    # Read a file, an environment, a parse tree and find the top-level
    # function definitions
    # How does this compare/differ from findFunctionDefs()
function(x, ...)
 standardGeneric("getFunctionDefs"))

setMethod("getFunctionDefs", "character",
    #
    #XXX vectorize in x.  See/use generalCharacterMethod ?
function(x, unlist = TRUE, recursive = FALSE, parse = TRUE, ...)
{
    if(!parse)
        return(NULL)
    
    if(file.exists(x)) {

        info = file.info(x)
        if(info$isdir[1]) {
            files = getRFiles(x)
            tmp = lapply(files, getFunctionDefs, recursive = recursive, ...)

            if(!recursive) { # !("recursive" %in% names(list(...))) && list(...)$recursive) {
                tt = table(unlist(lapply(tmp, names)))
                if(any(tt > 1))
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
function(x, env = new.env(parent = globalenv()), toSymbol = TRUE, recursive = FALSE, ...)
{
    if(length(x) == 0)
        return(list())

    if(recursive)
        return(getAllFunctionDefs(x, recursive = TRUE, ...))
    
    w = sapply(x, isFunAssign, toSymbol = toSymbol)

    if(toSymbol) {
        lapply(x[w], eval, env)
        as.list(env, all.names = TRUE)
    } else {
        defs = x[w]
        funs = lapply(x[w], function(e) eval(e[[3]], env))
        names(funs) = sapply(x[w], function(e)
                                     if(is.name(e[[2]]))
                                        as.character(e[[2]])
                                     else
                                        paste(deparse(e[[2]]), collapse = ""))
        funs
    }
})

#XXX Get the names on the elements in this and the .function method

tmp = function(x, parse = FALSE, ...)
{
    if(isFunAssign(x)) {
        ans = getFunctionDefs(x[[3]], parse = parse, ...)
        names(ans)[1] = as.character(x[[2]])
        ans
    } else if(is.name(x[[1]]) && as.character(x[[1]]) == "function") {
        unlist(c(x, getFunctionDefs(eval(x), parse = parse, ...)))
    } else
        unlist( lapply(as.list(x), getFunctionDefs, parse = parse, ...) )
}
setMethod("getFunctionDefs", "call", tmp)
#XXX Need to setOlClass("<-")
setMethod("getFunctionDefs", "<-", tmp)


setMethod("getFunctionDefs", "function",
function(x, parse = FALSE, ...)  #XXXX if don't have parse here, problems with 2 argument named parse in subsequent recursive calls
{
# browser()
    p = lapply(formals(x), getFunctionDefs, parse = parse, ...)
    unlist(c(p[sapply(p, length) > 0], getFunctionDefs(body(x), parse = parse, ...)))
})


#getFunctionDefs.default =
#function(x, ...)
#  list()


# Are the getFunctionDef - (no s) - legit?
#getFunctionDef.logical = getFunctionDef.character = getFunctionDef.numeric =
#getFunctionDefs.complex = getFunctionDefs.integer = getFunctionDefs.name =
    
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
    ans[sapply(ans, length) > 0]
}

#`getFunctionDefs.{` = `getFunctionDefs.=` = getFunctionDefs.if = getFunctionDefs.while = getFunctionDefs.for = `getFunctionDefs.(` =

setMethod("getFunctionDefs", "{", tmp)
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
# NULL


getFunctionDefs.call =   # see above.
function(x, ...)
   lapply(x, getFunctionDefs, ...)
}

#################################
