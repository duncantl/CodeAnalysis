getAllFunctionDefs =
    #
    # ??? How does this relate/differ from getFunctionDefs() ?
    #  and findFunctionDefs() in findFunctionDefs.R
    #
    # [done] currently setup for an expression not evaluated code.
    # [done] Also connect to getFunctionDefs and its methods for directories/files, environment, expression.
    #    use getFunctionDefs(, recursive = TRUE)
    #  So no need to export this function.
    #
    #
    # Compare to getFunctionDefs. This optionally descends recursively
    # to find nested functions. (getFunctionDefs() seems to do this also now.)
    #  So it does a lot more work than finding the top-level functions.
    # We can get nesting information for each function - the depth of nesting and the names of the ancestor/containing functions when they have names and are not anonymous funcions.
    # 
    #
    #
    # x is an expression, e.g.,  from parse(file).
    # def = quote(f <- function(x) {  y = x; g = function(w) w + x; g(3)}) 
    # getAllFunctionDefs(def)
    #
    #  def = quote(f <- function(x) {  y = x; if(FALSE) g = function(w) w + x; g(3)})
    # Anonymous fns
    # def2 = quote(function(x) lapply(x[sapply(x, function(x) class(x)[1]) == "bob"], function(x) x+1))
    #   first function is assigned to a name
    # def2 = quote(f <- function(x) lapply(x[sapply(x, function(x) class(x)[1]) == "bob"], function(x) x+1))    
    # TODO
    # 1. [test more on a script]  identify if(FALSE) and skip
    # 2. keep state of assignments and use as names - correctly.
    #
function(x, walker = mkFunFinder(recursive, nesting, skipIfFalse),
         recursive = TRUE, nesting = TRUE, skipIfFalse = TRUE, ...)
{
    walkCode(x, walker)
    walker$.funs()
}

if(FALSE) {
    e = parse("../tests/getAllFunDefs.R")
    getAllFunctionDefs(e)
}

mkFunFinder =
    #
    # recursive - logical. If TRUE, process parameters and body of function objects looking
    #  for nested function definitions.
    #
function(recursive = TRUE, nesting = TRUE, skipIfFalse = TRUE)
{
    defs = list()

    assignTo = character()
    nestLevel = integer()
    curDepth = 1L
    nestInfo = list()
    
    leaf = function(e, w) {
        #        message("leaf: ", class(e), e)
        ty = typeof(e)
        if(ty %in% c("expression", "pairlist", "language", "list"))
            w$call(e, w)
        else if(ty == "closure") {
            addFun(e, w)
#            procDefinedFun(e, w)
        }
       NULL
    }

    procDefinedFun =
        function(e, w) {
            walkCode(formals(e), w)
            walkCode(body(e), w)   
        }
    
    addFun = function(e, w) {
        defs[[ length(defs) + 1L ]] <<- e

        id = if(length(assignTo) > 0) getAsName(assignTo[[1]]) else NA
        names(defs)[length(defs)] <<- id
        
        if(nesting) {
            nestLevel <<- c(nestLevel , curDepth)
            nestInfo[[ length(nestInfo) + 1L ]] <<- assignTo
            curDepth <<- curDepth + 1L
        }
        
        if(recursive) {
            if(typeof(e) == "closure")
                procDefinedFun(e, w)
            else {
                walkCode(e[[2]], w)
                walkCode(e[[3]], w)
            }
        }
        if(nesting)
            curDepth <<- curDepth - 1L        

        NULL
    }

    pushAssign = function(val) 
        assignTo <<- c(val, assignTo)
    popAssign = function(val) 
        assignTo <<- assignTo[-1]    

    
    call = function(x, w){

             if(skipIfFalse && skipIfFalse(x, w))
                 return(NULL)

             if(typeof(x) == "closure")
                 browser()

              # This doesn't check if the RHS is a symbol.
             isAssign = is.name(x[[1]]) && as.character(x[[1]]) %in% c("=", "<-")
             if(isAssign) 
                 pushAssign(x[[2]])
             else
                 pushAssign(NA)

             
             for(ee in as.list(x))
                 if(!missing(ee))
                     walkCode(ee, w)
#             if(isAssign)
                 popAssign()
           }
        
        
    list(handler = function(e, w) {
        #        message("handler: ", class(e), e)
                      if(e == "function")
                          addFun
                      else
                          NULL
                  },
         leaf = leaf,
         call = call,
         .funs = function() {
                     if(nesting) {
                         attr(defs, "nestLevel") = nestLevel
                         attr(defs, "nestInfo") = lapply(nestInfo, function(x) naOmit(rev(sapply(x, getAsName))))
                     }
                     defs
             })
}
naOmit =
function(x)
    x[!is.na(x)]

getAsName =
function(e)
{
    if(is.logical(e) && is.na(e))
        return(NA)
    
    switch(typeof(e),
           "character" = e,
           "symbol" = as.character(e),
           "call" = paste(deparse(e), collapse = ""),
           "")
}



mkDataFrame =
function(info)
{
    if("nestLevel" %in% names(attributes(info))) {
        ans = data.frame(funName = names(info),
                         nestLevel = attr(info, "nestLevel"))
                    # was        nestNames = unlist(attr(info, "nestInfo"))
        ans$nestNames = attr(info, "nestInfo")
        ans$defn = unname(info)
    } else {
        ans = data.frame(funName = unlist(lapply(info, names)),
                         nestLevel = unlist(lapply(info, attr, "nestLevel")),
                         nestNames = unlist(lapply(info, function(x) lapply(attr(x, "nestInfo"), paste, collapse = ","))))
   

        ans$defn = unname(unlist(info, recursive = FALSE))
    }

    ans
}

