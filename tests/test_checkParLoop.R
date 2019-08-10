# Testing code for forLoop.R
library(CodeAnalysis)
library(testthat)

# Doesn't currently handle:
# - nested loops
# - this case of assigning into a constant:
#       for(i in 1:n) x[foo(i)] = 100
#   Too unusual, I didn't bother to implement it.


if(FALSE)
{

    # For development
    library(rstatic)
    library(CodeDepends)
    source("../R/checkParLoop.R")

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
#DTL: New version of rstatic leads to TRUE.
# I think this is probably parallelizable. and so FALSE is not correct.
# If we knew x and y were same-length/parallel vectors, then this parallelizable.
# More important, the i-th iteration doesn't depend on any other iteration
# That is the key we are looking for.
# ANd names(y)[i] doesn't depend on any other itartion.
p2 = checkParLoop(l2)
expect_true(p2[["result"]])


l2b = quote(
    for(i in 1:n){
        names(x)[foo(i)] = bar()
    }
    )
#DTL: Again, I think this is too conservative/restrictive.
# The i-th element of the result is independent of the other iterations.
p2b = checkParLoop(l2b)
expect_false(p2b[["result"]])


l3 = quote(
    for(i in 1:n){
        x[i] = foo(x[i])
        y[i] = bar()
    }
    )
# Again, the i-th iteration is independent of any other.
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
# See DTL.md
p5 = checkParLoop(l5)
expect_false(p5[["result"]])


l6 = quote(
    for(i in x){
        i = 1
        y[i] = foo()
    }
    )
# See DTL.md
p6 = checkParLoop(l6)
expect_false(p6[["result"]])


l7 = quote(
    for(i in x){
        y[i %% k] = foo(y[i %% k])
    }
    )
#See DTL.md
p7 = checkParLoop(l7)
expect_false(p7[["result"]])

l8 = quote(
    for(i in x){
        y[i] = foo(y[i])
    }
    )
#DTL: This looks too conservative. the i-th element of the result depends on the i-th element of y.
# Same as above - x and y are probably parallel vectors.
p8 = checkParLoop(l8)
expect_true(p8[["result"]])

p8b = checkParLoop(l8, checkIterator = TRUE)
expect_false(p8b[["result"]])


l9 = quote(
    for(i in x){
        y[, i] = foo()
    }
    )
# DTL: Again, this is the i-th iteration doesn't depend on any other iteration.
p9 = checkParLoop(l9)
expect_true(p9[["result"]])


l10 = quote(
    for(i in x){
        y[i, i] = foo()
    }
    )
#DTL: Again, I think it is TRUE. i-th iteration only depends on i-th iteration.
p10 = checkParLoop(l10)
expect_true(p10[["result"]])


l10b = quote(
    for(i in x){
        y[foo(i), bar(i)] = foobar()
    }
)
p10b = checkParLoop(l10b)
expect_false(p10b[["result"]])


# This case is more subtle.
# We know it can be parallelized because the ith iteration of the loop can only write into the ith row.
# Therefore it does not matter what the remaining indices are.
l11 = quote(
    for(i in x){
        y[i, foo(i)] = bar()
    }
    )
# DTL: Nice one!
p11 = checkParLoop(l11)
expect_true(p11[["result"]])
