# also 1:length(x) to seq(along = x)

ans = c()
for(i in x) {
   ans = c(ans, f(i))
}


ans = integer()
for(i in 1:length(x)) {
   ans[i] = f(x[i])
}


# Parallel vectors - mapply.
ans = c()
for(i in 1:length(x)) {
   ans = c(ans, f(x[i], y[i]))
}

