# also 1:length(x) to seq(along = x)

ans = c()
for(i in x) {
   ans = c(ans, f(i))
}


ans = integer()
for(i in 1:length(x)) {
   ans[i] = f(x[i])
}

ans = integer()
for(i in 2:length(x)) {
   ans[i] = f(x[i], x[i-1])
}


# start with a single value but still adding to the end of  a vector.
# Looping from 2 onwards.
ans = 0L
for(i in 2:length(x)) {
   ans[i] = f(x[i], ans[i-1])
}

ans = rep(NA, length(x))
ans[1] = 0L
for(i in 2:length(x)) {
   ans[i] = f(x[i], ans[i-1])
}
# Better
for(i in  (seq( along = x[-1])  + 1)) {
   ans[i] = f(x[i], ans[i-1])
}


# Parallel vectors - mapply.
ans = c()
for(i in 1:length(x)) {
   ans = c(ans, f(x[i], y[i]))
}

