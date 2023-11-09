f =
function(x)
   10*g(x)        

g =
function(a)
   a + z(a^2)

h =
function(x)
    f(g(x)) + 13

z = function(x)
       x*log(x)

library(CodeAnalysis)
callGraph(list(f = f, g = g, h = h))
callGraph(list(f = f, g = g, h = h), FALSE)
callGraph(list(f = f, g = g, h = h), recursive = TRUE)
callGraph(list(f = f, g = g, h = h), recursive = 2)
callGraph(list(f = f, g = g, h = h), recursive = 1)
callGraph(list(f = f, g = g, h = h), recursive = 0)


callGraph(quote(h(42)))

callGraph(quote(h(42)), recursive = FALSE)
callGraph(quote(h(42)), recursive = 1)


callGraph(g, name = "g")
#!! w/o name = "g", uses obj.

callGraph("package:CodeDepends")
