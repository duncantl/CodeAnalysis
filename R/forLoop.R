find_var2 = function(node, varname){
    rstatic::find_nodes(node, function(x)
                        is(x, "Symbol") && x$value == varname)
}



#' Determine If a Loop Can Be Made Parallel
#'
#' and tell the user why the loop is parallel or not.
#'
#' A loop can be made parallel if the order of the iterations do not matter.
#' This function errs on the side of being conservative; if it's not clear whether a loop is parallel or not, it will say that it is not.
#'
#' @return list with the following elements:
#'      - parallel (logical) can the for loop be parallel?
#'      - reason (character) human readable message for why the loop is or is not parallel
#'      - reason_code (character) short version of reason, for programming
parLoop = function(forloop)
{

    forloop = rstatic::to_ast(forloop)

    if(!is(l, "For")){
        stop("Not a for loop.")
    }

    deps = CodeDepends::getInputs(as_language(forloop$body))

    changed = c(deps@outputs, deps@updates)
    if(length(changed) == 0){
        return(list(
            parallel = TRUE,
            reason = "Loop does not change any variables.",
            reason_code = "NO_CHANGE"
            ))
    }

#    read_and_write = intersect(deps@inputs, deps@outputs)
#    if(0 < length(read_and_write)){
#        message(sprintf("Read after write (RAW) loop dependency in variables %s", 
#                        paste(read_and_write, collapse = ", ")))
#        return(FALSE)
#    }

    global_updates = intersect(deps@inputs, deps@updates)

    if(length(global_updates) == 0){
        return(FALSE)
    }

    # Case of loop such as: 
    # for(i in ...){
    #   x[[i]] = ...
    #   y[[i]] = ...
    # }
    # We can potentially work with this, but it isn't a priority, so just
    # give up and return the for loop.

    if(length(global_update) > 1){
        return(forloop)
    }

    # Verify loop has the form:
    # for(i in ...){
    #   ... g(x[[i]]) # It's ok if this kind of thing happens
    #   x[[i]] = ...
    # }

    variable = as.character(forloop$variable)
    body = forloop$body

    if(!onlyUseSimpleIndex(body, global_update, variable)){
        return(forloop)
    }

    braces = class(body) == "{"
    lastline = if(braces) body[[length(body)]] else body

    if(!isSimpleIndexAssign(lastline, global_update, variable)){
        return(forloop)
    }

}


#' Transfrom For Loop To Lapply
#'
#' Determine if a for loop can be parallelized, and if so transform it into
#' a call to \code{lapply}. 
#' 
#' @param forloop R language object with class \code{for}.
#' @return call R call to \code{lapply} if successful,
#'  otherwise the original forloop.
forLoopToLapply = function(forloop)
{

    #if(parLoop(forloop))

}


# Easy case: loop doesn't change anything
forLoopNoUpdates = function(forloop)
{
    out = substitute(lapply(iterator, function(variable) body)
        , as.list(forloop)
        )
    # The names of the function arguments are special.
    names(out[[c(3, 2)]]) = as.character(forloop$variable)

    out
}


# Harder case: loop does change things
forLoopWithUpdates = function(forloop, deps)
{

    # All the checks have passed, we can make the change.

    # Transform the for loop body into the function body
    rhs_index = 3
    if(braces){
        ll = length(body)
        rhs_of_lastline = body[[c(ll, rhs_index)]]
        body[[ll]] = rhs_of_lastline
    } else {
        body = lastline[[rhs_index]]
    }

    out = substitute(output[iterator] <- lapply(iterator, function(variable) body)
        , list(output = as.symbol(global_update)
               , iterator = forloop$iterator
               , variable = forloop$variable
               , body = body
        ))
    # The names of the function arguments are special.
    names(out[[c(3, 3, 2)]]) = as.character(forloop$variable)

    out
}


# Verify that the only usage of avar in expr is of the form
# avar[[variable]]
# @param avar character assignment variable
# @param variable character index variable
onlyUseSimpleIndex = function(expr, avar, variable)
{
    locs = find_var(expr, avar)

    for(loc in locs){
        lo = loc[-length(loc)]
        if(!isSimpleIndex(expr[[lo]], avar, variable)){
            return(FALSE)
        }
    }
    TRUE
}


# Verify that expr has the form
# avar[[variable]]
isSimpleIndex = function(expr, avar, variable, subset_fun = "[[")
{
    if((length(expr) == 3)
        && (expr[[1]] == subset_fun)
        && (expr[[2]] == avar)
        && (expr[[3]] == variable)
    ) TRUE else FALSE
}


# Verify that expr has the form
# avar[[variable]] = ...
isSimpleIndexAssign = function(expr, avar, variable
    , assign_funs = c("=", "<-"))
{
    f = as.character(expr[[1]])
    if(!(f %in% assign_funs)){
        return(FALSE)
    }

    lhs = expr[[2]]
    isSimpleIndex(lhs, avar, variable)
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
expect_true(parLoop(l0))


l1 = quote(
    for(i in 1:n){
        x = foo(x)
    }
)
expect_false(parLoop(l1))


l2 = quote(
    for(i in 1:n){
        names(x)[i] = names(y)[i]
    }
)
expect_true(parLoop(l2))




# Not working to allow getInputs to dispatch
#setOldClass("ASTNode")
#setMethod("getInputs", "ASTNode", function(e, ...){
#    callGeneric(as_language(e), ...)
#})
# Maybe relevant: https://github.com/r-lib/R6/issues/36



l0 = quote(for(i in seq(n)){
               x[i] = foo(y[i])
               bar(z[i])
    })
l = standardizeLoop(l0)


# Check when y is considered both an input and an output
getInputs(quote({
    z[i] = 5
    y = bar(y)
}))

# An update for CodeDepends can be from x[i] = ... or x = ... if it sees that x is defined
getInputs(quote({
    foo(y)
    y = bar(z)
    a = 100
    b[i] = 200
}))


}
