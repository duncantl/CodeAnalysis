# Read code in a package and determine what is there.
# What functions are
#  redefined
#  in if(FALSE) statements
# overwritten by methods - setMethod
#
#
# Part of the motivation for this is when we programmatically generate bindings for
# native code but we have already written functions and methods manually.
# Specifically, Rllvm.
#


# Find function definitions, generics and methods.

processDir =
function(dir, rfiles = list.files(dir, pattern = "\\.[RrSs]$"))
{
    e = lapply(rfiles, processFile)
    ans = do.call(rbind, e)
    structure(ans, class = c("PkgCodeInfo", class(ans)))
}


processFile =
function(f, code = parse(f))
{
    ans = lapply(seq(along = code), function(i) processExpr(code[[i]], i, f))
    tmp = do.call(rbind, ans)
    w = !sapply(ans, is.null)
    tmp$file = rep(f, sum(w))
    tmp$index = seq(along = ans)[w]
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
             # Allow for chaining  - a = b = c = function...
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
