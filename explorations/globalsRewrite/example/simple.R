
f =
function(x)
{
  alpha *x + beta
}

g =
function(y)
{
  y + rgamma(length(y), alpha)
}

main =
function(n, B = 999)
{
  replicate(B, { a = f(rnorm(n)); mean(g(a))})
}
