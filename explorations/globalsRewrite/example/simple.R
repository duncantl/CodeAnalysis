
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

# Add example of nested functions within a top-level function
# and how they access non-locals in the parent function(s).
