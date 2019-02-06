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


# Example 3 - Truly iterative, cannot transform
############################################################
ans = rep(NA, length.out = length(x))
ans[1] = 1
for(i in seq_along(x)[-1L]) {
    ans[i] = f(ans[i - 1])
}


# Example 4 - I actually wrote this code (inside the function
# removeDescendants) because it was more clear to me to express it in a
# loop. It conditionally updates a variable based on the value computed
# inside a loop.
############################################################
for(i in seq(N)){
    child = strings[i]
    matches = startsWith(child, strings)
    # Always matches itself, so we need more than 1
    if(sum(matches) > 1){
        ancestors[i] = FALSE
    }
}

# We could transform it into an apply style statement in the following way:
condition = sapply(seq(N), function(i){
    child = strings[i]
    matches = startsWith(child, strings)
    sum(matches) > 1
})
ancestors = ifelse(condition, FALSE, ancestors)

# But this code pattern and transformation seems pretty specialized, and it
# isn't clear that this is worth doing.
