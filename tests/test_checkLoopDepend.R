if(FALSE) {
# Testing code for forLoop.R
library(CodeAnalysis)
library(testthat)

# Doesn't currently handle:
# - nested loops
# - this case of assigning into a constant:
#       for(i in 1:n) x[foo(i)] = 100
#   Too unusual, I didn't bother to implement it.
# Fails
#
# for(i in seq(along = x)[-1]) x[i] = 2*x[i-1]
#


if(FALSE)
{

    # For development
    library(testthat)
    library(rstatic)
    library(CodeDepends)
    source("../R/checkLoopDepend.R")

}

l0 = quote(
    for(i in 1:n){
        foo(i)
    }
)
p0 = checkLoopDepend(l0)
stopifnot(p0[["result"]])


l1 = quote(
    for(i in 1:n){
        x = foo(x)
    }
)
p1 = checkLoopDepend(l1)
expect_false(p1[["result"]])


l2 = quote(
    for(i in 1:n){
        names(x)[i] = names(y)[i]
    }
    )
#DTL:  I think this is probably parallelizable. and so FALSE is not correct.
# If we knew x and y were same-length/parallel vectors, then this parallelizable.
# More important, the i-th iteration doesn't depend on any other iteration
# That is the key we are looking for.
# ANd names(y)[i] doesn't depend on any other itartion.
# CF: Agree
p2 = checkLoopDepend(l2)
expect_true(p2[["result"]])


l2b = quote(
    for(i in 1:n){
        names(x)[foo(i)] = bar()
    }
    )
#DTL: Again, I think this is too conservative/restrictive.
# CF: If we know that bar() will always return the same thing, then yes, it can be parallel.
#   Or if we know that:
#           1 < foo(i + 1) - foo(i) 
#   then yes, it can be parallel.
#   But without those assumptions it could happen that foo(i) = 1 for all i and bar() cycles through `letters`.
#   Then we don't know if the final name should be "a" or "z".
p2b = checkLoopDepend(l2b)
expect_false(p2b[["result"]])


l3 = quote(
    for(i in 1:n){
        x[i] = foo(x[i])
        y[i] = bar()
    }
    )
# Again, the i-th iteration is independent of any other.
# CF: Agree
p3 = checkLoopDepend(l3)
expect_true(p3[["result"]])

p3b = checkLoopDepend(l3, checkIterator = TRUE)
expect_true(p3b[["result"]])


l4 = quote(
    for(i in x){
        tmp = foo()
        f(tmp, i)
    }
)
p4 = checkLoopDepend(l4)
expect_true(p4[["result"]])

p4b = checkLoopDepend(l4, checkIterator = TRUE)
expect_true(p4[["result"]])


l5 = quote(
    for(i in x){
        tmp = y[i]
        z[tmp] = foo()
    }
    )
# See DTL.md
p5 = checkLoopDepend(l5)
expect_false(p5[["result"]])


# CF: If we determine that `foo()` is loop invariant and pull it out of the loop first, then this becomes the same as 5c.
l5b = quote(
    for(i in x){
        z[y[i]] = foo()
    }
    )
p5b = checkLoopDepend(l5b)
expect_false(p5b[["result"]])


# TODO: Currently fails
l5c = quote(
    for(i in x){
        z[y[i]] = const
    }
    )
p5c = checkLoopDepend(l5c)
expect_true(p5c[["result"]])


l5d = quote(
    for(i in x){
        const = foo(i)
        z[y[i]] = const
    }
    )
p5d = checkLoopDepend(l5d)
expect_false(p5d[["result"]])


l6 = quote(
    for(i in x){
        i = 1
        y[i] = foo()
    }
    )
# See DTL.md
p6 = checkLoopDepend(l6)
expect_false(p6[["result"]])


l7 = quote(
    for(i in x){
        y[i %% k] = foo(y[i %% k])
    }
    )
#See DTL.md
p7 = checkLoopDepend(l7)
expect_false(p7[["result"]])

l8 = quote(
    for(i in x){
        y[i] = foo(y[i])
    }
    )
#DTL: This looks too conservative. the i-th element of the result depends on the i-th element of y.
# Same as above - x and y are probably parallel vectors.
# CF: Agree
p8 = checkLoopDepend(l8)
expect_true(p8[["result"]])

p8b = checkLoopDepend(l8, checkIterator = TRUE)
expect_false(p8b[["result"]])


l9 = quote(
    for(i in x){
        y[, i] = foo()
    }
    )
# DTL: Again, this is the i-th iteration doesn't depend on any other iteration.
p9 = checkLoopDepend(l9)
# CF: Agree
expect_true(p9[["result"]])


l10 = quote(
    for(i in x){
        y[i, i] = foo()
    }
    )
#DTL: Again, I think it is TRUE. i-th iteration only depends on i-th iteration.
# CF: Agree
p10 = checkLoopDepend(l10)
expect_true(p10[["result"]])


l10b = quote(
    for(i in x){
        y[foo(i), bar(i)] = foobar()
    }
)
p10b = checkLoopDepend(l10b)
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
p11 = checkLoopDepend(l11)
expect_true(p11[["result"]])


# Examples 12 - 15 are from QTL/simulate.R 

l12 = quote(
    for(i in 1:nchr(cross)) {
        o <- grep("^QTL[0-9]+", colnames(cross$geno[[i]]$data))
        if(length(o) != 0) {
            # Iteratively build up qtlgeno- this is where the RAW dependency comes in.
            qtlgeno <- cbind(qtlgeno, cross$geno[[i]]$data[,o,drop=FALSE])
            cross$geno[[i]]$data <- cross$geno[[i]]$data[,-o,drop=FALSE]
            if(is.matrix(cross$geno[[i]]$map))
                cross$geno[[i]]$map <- cross$geno[[i]]$map[,-o,drop=FALSE]
            else
                cross$geno[[i]]$map <- cross$geno[[i]]$map[-o]
        }
    })
p12 = checkLoopDepend(l12)
expect_equal(p12[["reasonCode"]], "RAW")


l13 = quote(for(i in 1:nchr(cross))
        storage.mode(cross$geno[[i]]$data) <- "integer")
p13 = checkLoopDepend(l13)
expect_true(p13[["result"]])


l14 = quote(        for(i in 1:n.qtl) {
            temp <- map[[model[i,1]]]
            if(model[i,2] < min(temp)) {
                temp <- c(model[i,2],temp)
                names(temp)[1] <- paste("QTL",i,sep="")
            }
            else if(model[i,2] > max(temp)) {
                temp <- c(temp,model[i,2])
                names(temp)[length(temp)] <- paste("QTL",i,sep="")
            }
            else {
                j <- max((seq(along=temp))[temp < model[i,2]])
                temp <- c(temp[1:j],model[i,2],temp[(j+1):length(temp)])
                names(temp)[j+1] <- paste("QTL",i,sep="")
            }
            map[[model[i,1]]] <- temp
        })
p14 = checkLoopDepend(l14)
expect_false(p14[["result"]])


l15 = quote(for (i in 1:max(ctrl$seed)) {
    ind <- ctrl$aID[ctrl$seed == i]
    buildSeed <- list()
    for (j in seq_along(ind)) {
        print(ind[j])
        fileIn <- list.files("./", pattern = paste0("_", ind[j],
            ".rds"), full.names = T)
        tmp <- readRDS(fileIn)
        buildSeed[[j]] <- tmp
    }
    tmpSeed <- do.call(cbind.data.frame, buildSeed)
    buildOut[[i]] <- tmpSeed
})
p15 = checkLoopDepend(l15)
expect_true(p15[["result"]])
} else
    message("skipping test_checkLookDepend.R")
