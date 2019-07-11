# Testing code for forLoop.R


if(FALSE)
{

    # For development
    library(rstatic)
    library(CodeDepends)
    library(testthat)
    source("../R/forLoop.R")

}

l0 = quote(
    for(i in 1:n){
        foo(i)
    }
)
p0 = checkParLoop(l0)
stopifnot(p0[["result"]])


l1 = quote(
    for(i in 1:n){
        x = foo(x)
    }
)
p1 = checkParLoop(l1)
expect_false(p1[["result"]])


l2 = quote(
    for(i in 1:n){
        names(x)[i] = names(y)[i]
    }
)
p2 = checkParLoop(l2)
expect_true(p2[["result"]])


l2b = quote(
    for(i in 1:n){
        names(x)[foo(i)] = bar()
    }
)
p2b = checkParLoop(l2b)
expect_false(p2b[["result"]])


l3 = quote(
    for(i in 1:n){
        x[i] = foo(x[i])
        y[i] = bar()
    }
)
p3 = checkParLoop(l3)
expect_true(p3[["result"]])

p3b = checkParLoop(l3, checkIterator = TRUE)
expect_true(p3b[["result"]])


l4 = quote(
    for(i in x){
        tmp = foo()
        f(tmp, i)
    }
)
p4 = checkParLoop(l4)
expect_true(p4[["result"]])

p4b = checkParLoop(l4, checkIterator = TRUE)
expect_true(p4[["result"]])


l5 = quote(
    for(i in x){
        tmp = y[i]
        z[tmp] = foo()
    }
)
p5 = checkParLoop(l5)
expect_false(p5[["result"]])


l6 = quote(
    for(i in x){
        i = 1
        y[i] = foo()
    }
)
p6 = checkParLoop(l6)
expect_false(p6[["result"]])


l7 = quote(
    for(i in x){
        y[i %% k] = foo(y[i %% k])
    }
)
p7 = checkParLoop(l7)
expect_false(p7[["result"]])

l8 = quote(
    for(i in x){
        y[i] = foo(y[i])
    }
)
p8 = checkParLoop(l8)
expect_true(p8[["result"]])

p8b = checkParLoop(l8, checkIterator = TRUE)
expect_false(p8b[["result"]])


l9 = quote(
    for(i in x){
        y[, i] = foo()
    }
)
p9 = checkParLoop(l9)
expect_true(p9[["result"]])


l10 = quote(
    for(i in x){
        y[i, i] = foo()
    }
)
p10 = checkParLoop(l10)
expect_true(p10[["result"]])


# This case is more subtle.
# We know it can be parallelized because the ith iteration of the loop can only write into the ith row.
# Therefore it does not matter what the remaining indices are.
l11 = quote(
    for(i in x){
        y[i, foo(i)] = bar()
    }
)
p11 = checkParLoop(l11)
expect_true(p11[["result"]])


