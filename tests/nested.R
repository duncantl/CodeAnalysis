library(CodeAnalysis)
f =
function(x, y)
{
    a = x + y
    g = function(w)
            w + a / x^2

    g(300)
}

v = getGlobals(f)
