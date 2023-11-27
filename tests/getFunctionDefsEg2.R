foo = function(x)
    x + pi

bar = function(x, y) {
         w = x > 0 
         cor(x[w], y[w])
      }


x = list()
x$fun = function() 1

top =
function(x, y, doit = function(a) bar(a), ...)
{
    foo = function(a) {
        w = sapply(a, function(x) inherits(x, "list") && length(x) > 0)
        sapply(a[w], function(x) length(doit(x)))
    }

    mapply(function(i, v) { print(i); foo(v)}, x,  y)
}

f = g =
function(a, b)
        a + b

vec <- Vectorize(f)
vec2 = Vectorize(g)


if(FALSE) {
  false = function(a) a + 1    
}

setGeneric("myGeneric", function(x) standardGeneric("myGeneric"))


rec = function(x) {

    f = function(a)
        sin(cos(a))

    g = function(x) sum(log(x))

    f(x) + g(x)
}

