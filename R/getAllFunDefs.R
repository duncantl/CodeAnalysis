getAllFunctionDefs =
    # x is an expression, e.g.,  from parse(file).
    # def = quote(f <- function(x) {  y = x; g = function(w) w + x; g(3)}) 
    # getAllFunctionDefs(def)
    #
    #  def = quote(f <- function(x) {  y = x; if(FALSE) g = function(w) w + x; g(3)}) 
    #
    # TODO
    # 1. [test more on a script]  identify if(FALSE) and skip
    # 2. keep state of assignments and use as names - correctly.
    # 
function(x, walker = mkFunFinder(...), ...)
{
    walkCode(x, walker)
    walker$.funs()
}

mkFunFinder =
    #
    # recursive - logical. If TRUE, process parameters and body of function objects looking
    #  for nested function definitions.
    #
function(recursive = TRUE)
{
    defs = list()

    assignTo = character()
    
    leaf = function(e, w) {
#        message("leaf: ", class(e), e)
        if(typeof(e) == "expression")
            w$call(e, w)
       NULL
   }
    addFun = function(e, w) {
        defs[[ length(defs) + 1L ]] <<- e
        id = getAsName(assignTo[[1]])
        browser()        
        names(defs)[length(defs)] <<- id
        if(recursive) {
            walkCode(e[[2]], w)
            walkCode(e[[3]], w)
        }
        NULL
    }

    pushAssign = function(val) 
        assignTo <<- c(val, assignTo)
    popAssign = function(val) 
        assignTo <<- assignTo[-1]    

    
    call = function(x, w){
             message("call: ", class(x), " ", x)
             if(isIfFalse(x))
                 return()

             isAssign = is.name(x[[1]]) && as.character(x[[1]]) %in% c("=", "<-")
             if(isAssign) 
                 pushAssign(x[[2]])

             
             for(ee in as.list(x))
                 if(!missing(ee))
                     walkCode(ee, w)
             if(isAssign)
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
         .funs = function() defs)
}

getAsName =
function(e)
{
    switch(typeof(e),
           "character" = e,
           "symbol" = as.character(e),
           "call" = paste(deparse(e), collapse = ""),
           "")
}
