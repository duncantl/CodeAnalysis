# Example 1 - Simple, no dependence
############################################################

# Original:
ans = rep(NA, length.out = length(x))
for(i in seq_along(x)) {
   ans[i] = f(x[i])
}

# Desired Output:
ans = sapply(x, f)


# Example 2 - Second assignment inside loop, not used inside loop
############################################################

# Original:
ans = rep(NA, length.out = length(x))
for(i in seq_along(x)) {
   current = f(x[i])
   ans[i] = current
}
g(current)


