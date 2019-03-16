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

    names(forloop) = c("for", "ivar", "iterator", "body")

    deps = CodeDepends::getInputs(forloop$body)

    changed = c(deps@outputs, deps@updates)

    if(length(changed) > 0){
        forLoopWithUpdates(forloop, deps)
    } else {
        forLoopNoUpdates(forloop)
    }
}


# Easy case: loop doesn't change anything
forLoopNoUpdates = function(forloop)
{
    out = substitute(lapply(iterator, function(ivar) body)
        , as.list(forloop)
        )
    # The names of the function arguments are special.
    names(out[[c(3, 2)]]) = as.character(forloop$ivar)

    out
}


# Harder case: loop does change things
forLoopWithUpdates = function(forloop, deps)
{
    # read after write (RAW) loop dependency
    if(length(intersect(deps@inputs, deps@outputs) > 0)){
        return(forloop)
    }

    global_update = deps@updates

    # The code doesn't update global variables, so it can be parallelized.
    if(length(global_update) == 0){
        return(forLoopNoUpdates(forloop))
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

    ivar = as.character(forloop$ivar)
    body = forloop$body

    if(!onlyUseSimpleIndex(body, global_update, ivar)){
        return(forloop)
    }

    braces = class(body) == "{"
    lastline = if(braces) body[[length(body)]] else body

    if(!isSimpleIndexAssign(lastline, global_update, ivar)){
        return(forloop)
    }

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

    out = substitute(output[iterator] <- lapply(iterator, function(ivar) body)
        , list(output = as.symbol(global_update)
               , iterator = forloop$iterator
               , ivar = forloop$ivar
               , body = body
        ))
    # The names of the function arguments are special.
    names(out[[c(3, 3, 2)]]) = as.character(forloop$ivar)

    out
}


# Verify that the only usage of avar in expr is of the form
# avar[[ivar]]
# @param avar character assignment variable
# @param ivar character index variable
onlyUseSimpleIndex = function(expr, avar, ivar)
{
    locs = find_var(expr, avar)

    for(loc in locs){
        lo = loc[-length(loc)]
        if(!isSimpleIndex(expr[[lo]], avar, ivar)){
            return(FALSE)
        }
    }
    TRUE
}


# Verify that expr has the form
# avar[[ivar]]
isSimpleIndex = function(expr, avar, ivar, subset_fun = "[[")
{
    if((length(expr) == 3)
        && (expr[[1]] == subset_fun)
        && (expr[[2]] == avar)
        && (expr[[3]] == ivar)
    ) TRUE else FALSE
}


# Verify that expr has the form
# avar[[ivar]] = ...
isSimpleIndexAssign = function(expr, avar, ivar
    , assign_funs = c("=", "<-"))
{
    f = as.character(expr[[1]])
    if(!(f %in% assign_funs)){
        return(FALSE)
    }

    lhs = expr[[2]]
    isSimpleIndex(lhs, avar, ivar)
}
