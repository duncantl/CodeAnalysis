# also 1:length(x) to seq(along = x)

ans = c()
for(xi in x) {
   ans = c(ans, f(xi))
}

# transform to version that does preallocation:
#
# ans = rep(NA, length(x))
# for(i in seq_along(x)) {
#    ans[i] = f(x[i])
# }

# Then we can make the further transformation into an apply call:
#
# ans = sapply(x, f)

# And possibly a further transformation into a parallel apply:
#
# ans = simplify2array(parallel::mclapply(x, f))

# This might be a nice narrative thread to write about.


ans = integer()
for(i in 1:length(x)) {
   ans[i] = f(x[i])
}


# Parallel vectors - mapply.
ans = c()
for(i in 1:length(x)) {
   ans = c(ans, f(x[i], y[i]))
}

