# Testing code is in tests/test_forLoop.R
#
# Convention of these functions:
#
# find* functions return a list of rstatic nodes that match a particular condition, following the convention of rstatic::find_nodes
#
# check* functions return a list with the following elements:
#      - result (logical) did the check pass?
#      - reason (character) human readable message for why the code did or did not pass
#      - reasonCode (character) short version of reason, for programming
#
# Alternatively, I considered tacking reason, reasonCode on as attributes on to a logical.
# I decided against it because the list seemed simpler.



# @param node see rstatic::find_nodes
# @param vs rstatic Symbol to search for
findAllUpdates =
    # Is this for only complex assignments or does it include simple overwrites.
function(node, vs, includeOverwrite = FALSE) 
{
    k = findAssignsTo(node, vs)
    if(!includeOverwrite) {
        w = !sapply(k, function(x) is.name(x[[2]]))
        k = k[w]
    }

    k
}

findAssignsOverVar =
    # @param node R language object
    # @param vs rstatic symbol to search for
function(node, vs)
    findAssignsTo(node, vs, complex = FALSE)


varAppears =
function(node, var)
   var %in% getAllSymbols(node)



# Predicate function for findIndependentUpdates
#
# Duncan suggested this, and I realized that I've already written it.
# fixed_globals is a character vector of global variables that stay constant throughout the loop.
# Find those nodes that update based on the value of the iterator variable (ivar).
# This means that ivar appears within a [ or [[ on the left hand side of the assignment operator, for example:
#
# x[ivar] = ...
# x[, ivar] = ...
# x$foo$bar[[ivar]]$baz = ...
#
# @param v rstatic Symbol to search for
# @param ivar rstatic Symbol iterator variable: the j in for(j in ...)
independentUpdate = function(node, v, ivar, fixed_globals = character())
{
    if(is(node, "Replacement") && varAppears(node$write, v) ){

        # The order of these checks matters.

        rhs = rstatic::arg_value(node)
        if(is(rhs, "Symbol") && rhs$value %in% fixed_globals){
            #browser()
            # This case:
            # x[foo(i)] = const
            return(TRUE)
        }

        if(varAppears(node$write, ivar)){
            # This case:
            # x$foo$bar[[ivar]]$baz = ...
            return(TRUE)
        }
        index_args = rstatic::arg_index(node)
        index_same_as_ivar = sapply(index_args, `==`, ivar)

        # If it's a multidimensional array and at least one of the subscripts is the same as the iteration variable, then it doesn't matter what the rest of the subscripts are.
        if(any(index_same_as_ivar)){
            return(TRUE)
        }
    }
    FALSE
}



#' Determine If the Loop Iterations Depend on Each Other
#'
#' A loop can be made parallel if the iterations do not depend on each other.
#' A loop is not parallel if it has a true dependency on loop iterations.
#' There are several possible ways for a dependency to come up.
#' This functions stops and returns as soon as it finds one reason that the order matters.
#' The design errs on the side of being conservative; if it's not clear whether there is a dependency or not, it will report the dependency.
#' This is not actually the case - reports false positives!
#'
#' @param forloop for loop language object
#' @param checkIterator logical check that the iterator is a function call that is guaranteed to produce only unique values.
#'      This should be TRUE if you really want to be sure that the loop is parallelizable.
#' @param uniqueFuncs names of functions that will produce unique values.
#' @return list with the following elements:
#'      - result (logical) do the iterations of the loop depend on each other?
#'      - reason (character) human readable message for why the loop is or is not parallel
#'      - reasonCode (character) short version of reason, for programming
checkLoopDepend = function(forloop, checkIterator = FALSE, uniqueFuncs = c("seq", ":", "unique"))
{

    forloop = rstatic::to_ast(forloop)
    if(!is(forloop, "For"))
        stop("Not a for loop.")

    body = forloop$body
    ivar = forloop$variable    

    deps = CodeDepends::getInputs(rstatic::as_language(body))
    changed = c(deps@outputs, deps@updates)

    # The easy way out
    if(length(changed) == 0){
        return(list(
            result = TRUE
            , reason = "loop does not define or update any variables"
            , reasonCode = "NO_CHANGE"
        ))
    }

    if(ivar$value %in% changed){
        return(list(
            result = FALSE
            , reason = sprintf("iteration variable %s is changed within the body of the loop", ivar$value)
            , reasonCode = "ITERATION_VAR_CHANGE"
        ))
        # This would be OK if the loop body does not subsequently use the iterator variable in a subset assignment.
        # In that case it can be fixed by renaming the variable.
        # Indeed, there's really never any reason to redefine the iteration variable rather than just use a new variable.
    }

    #?? What is the meaning of fixed_globals? Why does it include ivar (i) which is not fixed?
    global_updates = intersect(deps@inputs, deps@updates)
    fixed_globals = setdiff(deps@inputs, changed)

    for(v in global_updates){
        tmp = checkVariableDependency(v, body, ivar, fixed_globals = fixed_globals)
        if(!tmp[["result"]]){
            return(tmp)
        }
    }

    if(checkIterator && 0 < length(global_updates)){
        tmp = checkUnique(forloop$iterator, uniqueFuncs)
        if(!tmp[["result"]]){
            return(tmp)
        }
    }

    return(list(
        result = TRUE
        , reason = "passed all tests for loop carried dependencies"
        , reasonCode = "PASS_LOOP_DEPEND"
    ))
}


# Check that an iterator is guaranteed to contain unique objects when evaluated.
checkUnique = function(iterator, uniqueFuncs)
{
    if(is(iterator, "Symbol")){
        return(list(
            result = FALSE
            , reason = sprintf("cannot be sure that the variable `%s` being looped over contains unique values", iterator$value)
            , reasonCode = "ITERATOR_FREE_VAR"
        ))
    } else if(is(iterator, "Call")) {
        if(!(iterator$fn$value %in% uniqueFuncs)){
        return(list(
            result = FALSE
            , reason = sprintf("the iterator is a call to the function `%s`, which may not produce unique values", iterator$fn$value)
            , reasonCode = "ITERATOR_UNKNOWN_FUNC"
        ))
        }
    } else {
        iter_msg = deparse(rstatic::as_language(iterator))
        return(list(
            result = FALSE
            , reason = sprintf("iterator `%s` is not a symbol or a call", iter_msg)
            , reasonCode = "ITERATOR_UNKNOWN"
        ))
    }
    list(
        result = TRUE
        , reason = "passed tests for uniqueness"
        , reasonCode = "PASS_UNIQUE"
    )
}


checkVariableDependency =
    #??? Comments describing what this does.
function(v, body, ivar, fixed_globals)
{
    vs = rstatic::Symbol$new(v)
    assigns_over_var = findAssignsOverVar(body, vs)
    if(0 < length(assigns_over_var)){
        return(list(
            result = FALSE
            , reason = sprintf("read after write dependency on variable `%s`", v)
            , reasonCode = "RAW"
        ))
    }

    all_updates = findAllUpdates(body, vs)
    ok_updates = find_nodes(body, independentUpdate, vs, ivar, fixed_globals)
    bad_updates = setdiff(all_updates, ok_updates)
# ok and bad are not helpful terms that convey why they are ok/bad.

    if(0 < length(bad_updates)){
        bad_up = body[[bad_updates[[1L]]]]
        bad_up_msg = deparse(rstatic::as_language(bad_up))
        return(list(
            result = FALSE
            , reason = c(sprintf("variable `%s` is assigned to using an index which is not the iterator variable in the loop:", v), bad_up_msg)
            , reasonCode = "COMPLEX_UPDATE"
        ))
    }

    # So this says everything is fine eventhough it may not be.  So (false) positive is the default.
    list(
        result = TRUE
        , reason = "passed variable dependency tests"
        , reasonCode = "PASS_DEPENDENCY"
    )
}
