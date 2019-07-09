# TODO: Use rstatic's symbols rather than checking values directly here, it will simplify code
find_var2 = function(node, varname){
    rstatic::find_nodes(node, function(x)
                        is(x, "Symbol") && x$value == varname)
}


# @param node see rstatic::find_nodes
# @param vs rstatic Symbol to search for
find_all_updates = function(node, vs){
    rstatic::find_nodes(node, function(x) is(x, "Replacement") && x$write == vs)
}


# @param node see rstatic::find_nodes
# @param vs rstatic Symbol to search for
find_assigns_over_var = function(node, vs){
    rstatic::find_nodes(node, function(x)
        is(x, "Assign") && !is(x, "Replacement") && x$write == vs)
}


# @param vs rstatic Symbol to search for
# @param iter_var rstatic Symbol iterator variable: the j in for(j in ...)
find_updates_var_with_iter_var = function(node, vs, iter_var){
    rstatic::find_nodes(node, function(x){
        if(is(x, "Replacement") && x$write == vs){
            # TODO: Write tests and generalize this to matrices and higher dimensional arrays, x[, i] = ...

            # This is the subset argument i as in x[i] = ...
            i = x$read$args$contents[[2L]]
            if(i == iter_var){
                return(TRUE)
            }
        }
        FALSE
    })
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
    var = forloop$variable

    if(!is(forloop, "For")){
        stop("Not a for loop.")
    }

    deps = CodeDepends::getInputs(as_language(body))
    changed = c(deps@outputs, deps@updates)

    if(length(changed) == 0){
        return(list(
            parallel = TRUE
            , reason = "loop does not define or update any variables"
            , reasonCode = "NO_CHANGE"
        ))
    }

    global_updates = intersect(deps@inputs, deps@updates)

    for(v in global_updates){
        vs = rstatic::Symbol$new(v)
        assigns_over_var = find_assigns_over_var(body, vs)
        if(0 < length(assigns_over_var)){
            return(list(
                parallel = FALSE
                , reason = sprintf("read after write dependency on variable `%s`", v)
                , reasonCode = "RAW"
            ))
        }

        all_updates = find_all_updates(body, vs)
        ok_updates = find_updates_var_with_iter_var(body, vs, var)
        bad_updates = setdiff(all_updates, ok_updates)
        if(0 < length(bad_updates)){
            bad_up = body[[bad_updates[[1L]]]]
            return(list(
                parallel = FALSE
                , reason = sprintf("variable `%s` is assigned to in a complex way: %s", v, bad_up)
                , reasonCode = "COMPLEX_UPDATE"
            ))

        }
    }

    return(list(
        parallel = TRUE
        , reason = "passed all tests for loop carried dependencies"
        , reasonCode = "PASS_TEST"
    ))

}



if(FALSE){

# Testing code

library(rstatic)
library(CodeDepends)
library(testthat)
source("forLoop.R")

# Passing
l0 = quote(
    for(i in 1:n){
        foo(i)
    }
)
p0 = parLoop(l0)
stopifnot(p0[["parallel"]])


# Passing
l1 = quote(
    for(i in 1:n){
        x = foo(x)
    }
)
p1 = parLoop(l1)
expect_false(p1[["parallel"]])


# Known failure
l2 = quote(
    for(i in 1:n){
        names(x)[i] = names(y)[i]
    }
)
p2 = parLoop(l2)
expect_true(p2[["parallel"]])


# Passing
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


l7 = quote(
    for(i in x){
        y[i %% k] = foo(y[i %% k])
    }
)
p7 = parLoop(l7)
expect_false(p7[["parallel"]])


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
