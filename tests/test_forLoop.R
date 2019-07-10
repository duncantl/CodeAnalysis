# Testing code for forLoop.R

library(rstatic)
library(CodeDepends)
library(testthat)
source("forLoop.R")

l0 = quote(
    for(i in 1:n){
        foo(i)
    }
)
p0 = parLoop(l0)
stopifnot(p0[["parallel"]])


l1 = quote(
    for(i in 1:n){
        x = foo(x)
    }
)
p1 = parLoop(l1)
expect_false(p1[["parallel"]])



if(FALSE){
# Known failure
l2 = quote(
    for(i in 1:n){
        names(x)[i] = names(y)[i]
    }
)
p2 = parLoop(l2)
expect_true(p2[["parallel"]])
}


l3 = quote(
    for(i in 1:n){
        x[i] = foo(x[i])
        y[i] = bar()
    }
)
p3 = parLoop(l3)
expect_true(p3[["parallel"]])

p3b = parLoop(l3, checkIterator = TRUE)
expect_true(p3b[["parallel"]])


l4 = quote(
    for(i in x){
        tmp = foo()
        f(tmp, i)
    }
)
p4 = parLoop(l4)
expect_true(p4[["parallel"]])

p4b = parLoop(l4, checkIterator = TRUE)
expect_true(p4[["parallel"]])


l5 = quote(
    for(i in x){
        tmp = y[i]
        z[tmp] = foo()
    }
)
p5 = parLoop(l5)
expect_false(p5[["parallel"]])


l6 = quote(
    for(i in x){
        i = 1
        y[i] = foo()
    }
)
p6 = parLoop(l6)
expect_false(p6[["parallel"]])


l7 = quote(
    for(i in x){
        y[i %% k] = foo(y[i %% k])
    }
)
p7 = parLoop(l7)
expect_false(p7[["parallel"]])

l8 = quote(
    for(i in x){
        y[i] = foo(y[i])
    }
)
p8 = parLoop(l8)
expect_true(p8[["parallel"]])

p8b = parLoop(l8, checkIterator = TRUE)
expect_false(p8b[["parallel"]])


l9 = quote(
    for(i in x){
        y[, i] = foo()
    }
)
p9 = parLoop(l9)
expect_true(p9[["parallel"]])


l10 = quote(
    for(i in x){
        y[i, i] = foo()
    }
)
p10 = parLoop(l10)
expect_true(p10[["parallel"]])


# This case is more subtle.
# We know it can be parallelized because the ith iteration of the loop can only write into the ith row.
# Therefore it does not matter what the remaining indices are.
l11 = quote(
    for(i in x){
        y[i, foo(i)] = bar()
    }
)
p11 = parLoop(l11)
expect_true(p11[["parallel"]])
