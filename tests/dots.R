library(CodeAnalysis)

bar = 
function(x, y = rep(1, length(x)), ...)
{
    a = list(...)
    plot(x, y, ...)    
    if(a[1] > 2)
        text(x, y, ...)
        
}


d = usesDots(bar)
stopifnot(length(d) == 4)

f =
function(x, ...)    
{
    x = list(...)
    bar(x, ...)
    bar(x, list(...))
}
