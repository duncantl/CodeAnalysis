library(CodeAnalysis)

foo = 
function(x)
{
    v = attr(x, "abc")
    attr(v, "xyz") = TRUE
    length(v)

    z = structure(x[1:10],
                  names = v,
                  class = "other",
                  dir = ".")
    
    class(v) = "bar"
    v
}

a = getAttributeNames(foo)
# we now preserve the order.
stopifnot(all(a == c("abc", "xyz", "names", "class", "dir")))


