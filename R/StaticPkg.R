# Read code in a package and determine what is there.
# What functions are
#  redefined
#  in if(FALSE) statements
# overwritten by methods - setMethod
#
#
# Part of the motivation for this is when we programmatically generate bindings for
# native code but we have already written functions and methods manually.
# We want to find out what already exists. Specifically, Rllvm.
# Also when we have code in one package A that uses code from another B, i.e.,
#  what do we need to import from B
# Also see getAllCalls() for the latter and there is code in package tools
# that does something related.


# Find function definitions, generics and methods.

# A dataframe describing the top-level elements in the code in a file, directory, expression.
# See also getFunctionDefs() and getAllCalls()


pkgCodeInfo = # processDir =
function(dir, rfiles = list.files(dir, pattern = "\\.[RrSs]$", full.names = TRUE))
{
    if(!file.info(dir)$isdir)
        return(fileCodeInfo(dir))
    
    e = lapply(rfiles, fileCodeInfo, addClass = FALSE)
    ans = do.call(rbind, e)
    structure(ans, class = c("PkgCodeInfo", class(ans)))
}


fileCodeInfo = processFile = 
    # Get better name.
    #
    # parse a file of R code and return a data.frame
    # describing the top-level expressions
    #
function(file, code = parse(file), addClass = TRUE)
{
    if(length(code) == 0)
        return(NULL)

    ans = lapply(seq(along = code), function(i) processExpr(code[[i]], i, file))
    ans = ans[ ! sapply(ans, is.null) ]
    if(length(ans) == 0)
       return(NULL)
       
    tmp = do.call(rbind, ans)
    if(nrow(tmp) == 0) {
        tmp$file = character()
        tmp$index = integer()
        return(tmp)
    }
    
    nr = sapply(ans, function(x) if(is.null(x)) 0 else nrow(x))
    tmp$file = rep(file, sum(nr))
    tmp$index = rep(seq(along.with = ans), nr)
    if(addClass)
        class(tmp) = c("FileCodeInfo", class(tmp))
    tmp
}


processExpr =
function(e, index, filename)
{
    op = NULL
    d = if(class(e) == "if") {
            br = e[- (1:2)]
            if(is.logical(e[[2]])) {
                if(e[[2]])
                    processExpr(br[[1]])
                else if(length(br) > 1)
                    processExpr(br[[2]])
                else
                    NULL
            }
            
        } else if(class(e) == "name") {
           data.frame(name = as.character(e), type = "symbol", stringsAsFactors = FALSE)
         } else if(class(e) %in% c("=", "<-")) {
             # Allow for chaining, e.g.m
             # a = b = c = function...
             ty = e[[3]]
             if(is.call(ty) && ty[[1]] == "function")
                 ty = "function"
             else {
                 ty = "call"
                 op = as.character(if(is.name(e[[3]])) e[[3]] else (e[[3]][[1]]))
             }
           data.frame(name = as.character(e[[2]]), type = ty)
       } else if(is.call(e)) {
           switch(as.character(e[[1]]),
               "setClass" = data.frame(name = e[[2]], type = "S4class", stringsAsFactors = FALSE),
               "setClassUnion" = data.frame(name = e[[2]], type = "ClassUnion", stringsAsFactors = FALSE),
                  "setOldClass" = data.frame(name = if(is.character(e[[2]])) e[[2]]
                                                     else e[[2]][[2]], type = "OldClass", stringsAsFactors = FALSE),
               "setMethod" = data.frame(name = e[[2]], type = "S4Method", stringsAsFactors = FALSE),
               "setGeneric" = data.frame(name = e[[2]], type = "S4generic", stringsAsFactors = FALSE),
               "setAs" = data.frame(name = e[[2]], type = "coerce", stringsAsFactors = FALSE),
                  default = stop("Don't recognize ", as.character(e[[1]]))
              )
       }

    if(!is.null(d)) {
        d$op = if(!is.null(op)) op else as.character(e[[1]])
        d$expr = list(I(e))
    }
    d
}
