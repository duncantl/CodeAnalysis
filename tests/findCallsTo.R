library(CodeAnalysis)

z = findCallsTo(read.dcf, "on.exit")
stopifnot(length(z) == 1)

z = findCallsTo(read.dcf, c("on.exit", ".Internal"))
stopifnot(length(z) == 2)

## pkg::foo

p = function(x) {
    y = base::rbind(x, x)
    z = rbind(x)
    foo(z)
    pkg::foo(x)
}

stopifnot(length(findCallsTo(p, c("rbind", "foo"))) == 4)

# won't find foo(z) because need pkg::foo
stopifnot(length(findCallsTo(p, c("rbind", "pkg::foo"))) == 3)


p2 = function(x) {
    do.call(base::rbind, x)
    lapply(x, pkg:::foo, other)
    lapply(x, foo, other)    
}
stopifnot(length(findCallsTo(p2, c("rbind", "foo"))) == 3)

# :: not ::: 
stopifnot(length(findCallsTo(p2, c("rbind", "pkg::foo"))) == 1)
stopifnot(length(findCallsTo(p2, c("pkg:::foo"))) == 1)


# isCallTo()

stopifnot(isCallTo(quote(do.call(rbind, x)), "rbind"))
stopifnot(!isCallTo(quote(do.call(rbind, x)), "rbind", character()))
stopifnot(!isCallTo(quote(do.call(rbind, x)), "rbind", FALSE))

stopifnot(isCallTo(quote(do.call(base::rbind, x)), "rbind"))

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
