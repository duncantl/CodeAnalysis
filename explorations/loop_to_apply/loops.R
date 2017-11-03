# Example 1 - Simple, no dependence
############################################################

# Original:
ans = rep(NA, length.out = length(x))
for(i in seq_along(x)) {
    ans[i] = f(x[i])
}

# Transformed:
ans = sapply(x, f)


# Example 1b - Extra argument
############################################################

# Original:
ans = rep(NA, length.out = length(x))
for(i in seq_along(x)) {
    ans[i] = f(x[i], z = 100)
}

# Transformed:
ans = sapply(x, f, z = 100)


# Example 2 - Intermediate variable used inside loop
#
# I often use intermediate variables to make my code easier to read and
# debug. This may be more complex than we care to go. It's interesting
# because the solution involves the intermediate step of reducing the
# problem to something equivalent to Example 1.
############################################################

# Original:
ans = rep(NA, length.out = length(x))
for(i in seq_along(x)) {
    tmp = f(x[i])
    ans[i] = g(tmp)
}


# Intermediate:
loopbody = function(xi)
{
    tmp = f(xi)
    g(tmp)
}
ans = rep(NA, length.out = length(x))
for(i in seq_along(x)) {
    ans[i] = loopbody(x[i])
}


# Transformed:
loopbody = function(xi)
{
    tmp = f(xi)
    g(tmp)
}
ans = sapply(x, loopbody)
