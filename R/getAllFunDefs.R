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

    leaf = function(e, w) {
#        message("leaf: ", class(e), e)
        if(typeof(e) == "expression")
           w$call(e, w)
       NULL
   }
    addFun = function(e, w) {
        defs[[ length(defs) + 1L ]] <<- e
        if(recursive) {
            walkCode(e[[2]], w)
            walkCode(e[[3]], w)
        }
        NULL
    }
        
    list(handler = function(e, w) {
#        message("handler: ", class(e), e)        
                      if(e == "function")
                          addFun
                      else
                          NULL
                  },
         leaf = leaf,
         call = function(x, w){
             message("call: ", class(x), " ", x)
             if(isIfFalse(x))
                 return()
             
                     for (ee in as.list(x))
                         if (!missing(ee))
                             walkCode(ee, w)
                 },
         .funs = function() defs)
}
