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
