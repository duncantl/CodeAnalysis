# Testing code is in tests/test_forLoop.R


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


# Find those nodes that update based strictly on the value of the iterator variable
#
# @param vs rstatic Symbol to search for
# @param iter_var rstatic Symbol iterator variable: the j in for(j in ...)
find_updates_var_with_iter_var = function(node, vs, iter_var){
    rstatic::find_nodes(node, function(x){
        if(is(x, "Replacement") && x$write == vs){
            # If it's a multidimensional array and any of the subscripts are the same as the iteration variable then it doesn't matter what the rest of the subscripts are.

            args = x$read$args$contents
            index_args = args[-c(1L, length(args))]
            index_same_as_iter_var = sapply(index_args, `==`, iter_var)

            if(any(index_same_as_iter_var)){
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

    # The easy way out.
    if(length(changed) == 0){
        return(list(
            parallel = TRUE
            , reason = "loop does not define or update any variables"
            , reasonCode = "NO_CHANGE"
        ))
    }

    if(var$value %in% changed){
        return(list(
            parallel = FALSE
            , reason = sprintf("iteration variable %s is changed within the body of the loop", var$value)
            , reasonCode = "ITERATION_VAR_CHANGE"
        ))
        # This would be OK if the loop body does not subsequently use the iterator variable in a subset assignment.
        # In that case it can be fixed by renaming the variable.
        # Indeed, there's really never any reason to redefine the iteration variable rather than just use a new variable.
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
            bad_up_msg = deparse(as_language(bad_up))
            return(list(
                parallel = FALSE
                , reason = sprintf("variable `%s` is assigned to using an index which is not the iterator variable in the loop: %s", v, bad_up_msg)
                , reasonCode = "COMPLEX_UPDATE"
            ))
        }
    }

    if(checkIterator && 0 < length(global_updates)){
        iterator = forloop$iterator
        if(is(iterator, "Symbol")){
            return(list(
                parallel = FALSE
                , reason = sprintf("cannot be sure that the variable `%s` being looped over contains unique values", iterator$value)
                , reasonCode = "ITERATOR_FREE_VAR"
            ))
        } else if(is(iterator, "Call")) {
            if(!(iterator$fn$value %in% uniqueFuncs)){
            return(list(
                parallel = FALSE
                , reason = sprintf("the iterator is a call to the function `%s`, which may not produce unique values", iterator$fn$value)
                , reasonCode = "ITERATOR_UNKNOWN_FUNC"
            ))
            }
        } else {
            iter_msg = deparse(as_language(iterator))
            return(list(
                parallel = FALSE
                , reason = sprintf("iterator `%s` is not a symbol or a call", iter_msg)
                , reasonCode = "ITERATOR_UNKNOWN"
            ))
        }
    }

    return(list(
        parallel = TRUE
        , reason = "passed all tests for loop carried dependencies"
        , reasonCode = "PASS_TEST"
    ))
}
