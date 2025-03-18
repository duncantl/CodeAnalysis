f1 =
function(x)
{
    ans = c(low = min(x), high = max(x))
    class(ans) = c("Extent")
    ans
}

f2 =
function(x)
{
    ans = c(low = min(x), high = max(x))
    class(ans) = "Extent"
    ans
}


f3 =
function(x)
{
    ans = c(low = min(x), high = max(x))
    class(ans) = c("Extent", "1D")
    ans
}


f3.5 =
function(x)
{
    sym = "1D"
    ans = c(low = min(x), high = max(x))
    class(ans) = c("Extent", sym)
    ans
}


f4 =
function(x)
    structure(c(low = min(x), high = max(x)), class = c("Extent", "1D"))

f4.5 =
function(x) {
    sym = "1D"       
    structure(c(low = min(x), high = max(x)), class = c("Extent", sym))
}


f5 =
function(x)
{
    sym = "Bob"
    ans = c(low = min(x), high = max(x))
    class(ans) = c("Extent", sym)
    ans
}

f6 =
function(x)
{
    sym = "Bob"
    ans = c(low = min(x), high = max(x))
    class(ans) = if(length(x)) "Directory" else "File"
    ans
}
