library(CodeAnalysis)

funs = list(
    foo = function(x, g, f)    
        lapply(split(x, g), f),

    foo1 = function(files, f) {
        tmp = lapply(files, f)
        do.call(rbind, tmp)
    },

    foo2 = function(x, f)    
        f(x + 1) + 2    
   ,
    foo3 = function(files, f, g) {
        tmp = lapply(files, f)
        do.call(g, tmp)
    },

    foo4 = function(x, f, g) {
        y  = f(files)
        tmp = mapply(g, x, y)
        do.call(rbind, tmp)
    }        
)

exp = list(foo = "f",
           foo1 = "f",
           foo2 = "f",
           foo3 = c("f", "g"),
           foo4 = c("f", "g")
          )

stopifnot(length(funs) == length(exp))
mapply(function(fun, ans)
           identical(findCallsParam(fun), ans),
       funs, exp)
         
