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

h =
function(x)
{
   XML:::trim(x)
}


m =
function(n)
{
    ans = 1
    function(x) {
       ans <<- ans + (x * n +  a)
    }
}



fails = function()
{
   g = function(x) x + a
   
   ans = g(10)
   
   a = 20
   
   ans
}



fails2 = function(n)
{
   g = function(x) x + a
   
   ans = g(10)
   
   if(n > 10)
       a = 20
   
   ans
}
