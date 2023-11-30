fib = function(n) {
    if(n < 2)
        n
    else
        fib(n - 1) + fib(n - 2)
}

mkLik =
function(x)    
{
    function(mu = 0, sd = 1)
        prod(dnorm(x, mu, sd))
}


