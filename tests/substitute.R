library(CodeAnalysis)

s1 = 
function(a, b)
{
    substitute(a + val, list(val = a + b))
}

s2 =
function(a, b)
{
    substitute(a + val, list(val = a + x))
}

s3 =
function(a, b)
{
    x = 1
    substitute(a + val, list(val = a + x))
}

tmp = lapply(list(s1, s2, s3), function(x) getGlobals(x)$variables)
stopifnot(tmp[[2]] == "x")
stopifnot(all(sapply(tmp[c(1, 3)], length) == 0))


b1 =
function(x)    
{
    bquote(x == x)
    bquote(x == .(x))
}

b2 =
function(x)    
{
    bquote(x == .(x + a))
}


b3 =
function(x, a)    
{
    bquote(x == .(x + a))
}

b4 =
function(x, env)    
{
    bquote(x == .(x + a), env)
}
o = getGlobals(b4)
stopifnot(length(o$variables) == 0)

b5 =
function(x, env0)    
{
    bquote(x == .(x + a), env, splice = doSplice)
}
o = getGlobals(b5)
stopifnot(all(o$variables == c("env", "doSplice")))
