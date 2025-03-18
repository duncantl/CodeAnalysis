
e = quote(function(x, y) x + y)
stopifnot(evalsToFunction2(e))


e = quote({
    fn = function(x, y) x + y
    environment(fn) = globalenv()
    fn
    })
evalsToFunction2(e)

e = quote({
    fn = function(x, y) x + y
    environment(fn) = globalenv()
    fn = list(1, 2)
    fn
    })
evalsToFunction2(e)


e = quote({
    x = 1L
    y = 2
    attr(x, "xyz") = 3
    x
    })
getReturnType(e)


e = quote(
   x <- if(a)
        function(x, y) x + y
    else
        function(x, y) x/y*2        
    )
evalsToFunction2(e)
CodeAnalysis:::evalsToFunction2(e)





e = quote({
    x = 1
    y = 2
    sin(x/y)
  })
getReturnType(e)


