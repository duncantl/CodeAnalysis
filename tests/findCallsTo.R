library(CodeAnalysis)

z = findCallsTo(read.dcf, "on.exit")
stopifnot(length(z) == 1)

z = findCallsTo(read.dcf, c("on.exit", ".Internal"))
stopifnot(length(z) == 2)


## Indirect calls

f = function(x, y) {
    y = rbind(y)
    o = do.call(rbind, x)
    z = lapply(y, rbind)
    x[idx] = lapply(x[idx], as.numeric)
    x
}
z = findCallsTo(f, c("rbind", "as.numeric"))
stopifnot(length(z) == 4)
z = findCallsTo(f, c("rbind", "as.numeric"), indirect = FALSE)
stopifnot(length(z) == 1)
z = findCallsTo(f, c("rbind", "as.numeric"), indirect = TRUE)
stopifnot(length(z) == 4)
