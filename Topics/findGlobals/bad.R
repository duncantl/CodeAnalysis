f =
function(x)
{
   y = x + a
   a = 1
   y
}

g =
function(x)
{
   stats::quantile(x)
}


o =
function(a)
{
    set = function(val)
        a <<- val

    list(set = set, get = function() a)
}
