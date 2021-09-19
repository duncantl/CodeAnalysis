if(FALSE) {
  getIfValue(findIfInFun(foo)[[1]])
}

foo =
function(x)
{
    y = 2
    if(length(x))
        y = y + 3
    else
        y = 10

    y
}

foo2 =
function(x)
{
    y = 2L
    names(y) = "a"
    y[2] = 11 # changes to numeric.
    if(length(x))
        y = y + 3
    else
        y = 10

    y
}

bar =
function(x)
{
    if(length(x))
        x + 3
    else
        10
}
