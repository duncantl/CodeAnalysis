foo =
function(x, y)
{
    a = 1
    if(length(x) > 0)
        a = 2


    if(length(y) > length(x)) {

        length(x) = length(y)
        a = rep(a, length(x))
        b = 2
    } else 
        b = 3

    x + y  - a + b
}

