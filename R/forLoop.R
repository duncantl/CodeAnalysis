find_var2 = function(node, varname){
    rstatic::find_nodes(node, function(x)
                        is(x, "Symbol") && x$value == varname)
}

assigns_into_var = function(node, varname){
    finds = rstatic::find_nodes(node, function(x)
                        is(x, "Assign") && 
                        is(x$write, "Symbol") &&
                        x$write$value == varname
                    )
    0 < length(finds)
}



#' Determine If a Loop Can Be Made Parallel
#'
#' and tell the user why the loop is parallel or not.
#'
#' A loop can be made parallel if the order of the iterations do not matter.
#' A loop is not parallel if it has a true dependency on loop iterations.
#' There might be many things that stop a loop from being parallel.
#' This functions stops and returns as soon as it finds one reason.
#' The design errs on the side of being conservative; if it's not clear whether a loop is parallel or not, it will say that it is not.
#'
#' @param forloop for loop language object
#' @param checkIterator logical check that the iterator is a function call that is guaranteed to produce only unique values.
#'      This should be TRUE if you really want to be sure that the loop is parallelizable.
#' @param uniqueFuncs names of functions that will produce unique values.
#' @return list with the following elements:
#'      - parallel (logical) can the for loop be parallel?
#'      - reason (character) human readable message for why the loop is or is not parallel
#'      - reasonCode (character) short version of reason, for programming
parLoop = function(forloop, checkIterator = FALSE, uniqueFuncs = c("seq", ":", "unique"))
{

    forloop = rstatic::to_ast(forloop)
    body = forloop$body

    if(!is(forloop, "For")){
        stop("Not a for loop.")
    }

    deps = CodeDepends::getInputs(as_language(body))
    changed = c(deps@outputs, deps@updates)

    if(length(changed) == 0){
        return(list(
            parallel = TRUE,
            reason = "loop does not define or update any variables",
            reasonCode = "NO_CHANGE"
        ))
    }

    global_updates = intersect(deps@inputs, deps@updates)
    for(v in global_updates){
        if(assigns_into_var(body, v)){
            return(list(
                parallel = FALSE,
                reason = sprintf("read after write dependency on variable `%s`", v),
                reasonCode = "RAW"
            ))
        }
    }

}



if(FALSE){

# Scratch
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
expect_true(p0[["parallel"]])


l1 = quote(
    for(i in 1:n){
        x = foo(x)
    }
)
p1 = parLoop(l1)
expect_false(p1[["parallel"]])


l2 = quote(
    for(i in 1:n){
        names(x)[i] = names(y)[i]
    }
)
p2 = parLoop(l2)
expect_true(p2[["parallel"]])


l3 = quote(
    for(i in 1:n){
        x[i] = foo()
        y[i] = bar()
    }
)
p3 = parLoop(l3)
expect_true(p3[["parallel"]])


l4 = quote(
    for(i in x){
        tmp = foo()
        f(tmp, i)
    }
)
p4 = parLoop(l4)
expect_false(p4[["parallel"]])


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

}



if(FALSE){

# Not working to allow getInputs to dispatch
#setOldClass("ASTNode")
#setMethod("getInputs", "ASTNode", function(e, ...){
#    callGeneric(as_language(e), ...)
#})
# Maybe relevant: https://github.com/r-lib/R6/issues/36

q = quote_ast(assign("x", 1))

# Check when y is considered both an input and an output
getInputs(quote({
    assign("x", 50)
    y = bar(y)
    z = 5
}))

# An update for CodeDepends can be from x[i] = ... or x = ... if it sees that x is defined
getInputs(quote({
    foo(y)
    y = bar(z)
    a = 100
    b[i] = 200
}))


}
