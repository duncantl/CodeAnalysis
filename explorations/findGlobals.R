library(rstatic)

ApplyFunNames = c("sapply", "lapply", "vapply", "apply", "mapply")

genCollector =
function(existingVars = character())
{
    params = character()
    localVars = existingVars
    funs = character()
    vars = character()
    loopVar = character()

    process = function(el) {

        if(is(el, "Parameter"))
              # Already in params.
            return(TRUE)

        # if the symbol is part of ns::fun identifying the function in call ns::fun(.....)
        # then ignore as we deal with this below at the :: or ::: object.
        if(is(el, "Symbol") && is(el$parent, "Call") && el$parent$fn$name %in% c("::", ":::"))
            return(TRUE)
        
#        if(is(el, "Symbol") && el$name == "order") browser()

        
        if(is(el, "Symbol") && is(el$parent, "For") && identical(el, el$parent$ivar)) {
             # define the loop variable temporarily
            addLocalVar(el$name)
        }
        
        if(is(el, "Function")) {
#browser()            
            tmp = findGlobals(ast = el, merge = FALSE, existingVars = c(params, localVars))
            vars <<- c(vars, tmp$vars)
            funs <<- c(funs, tmp$functions)
            return(FALSE)
        }
        
        if(is(el, "Symbol") && is(el$parent, "Assign") && identical(el, el$parent$write)) {
            addLocalVar(el$name)
            return(TRUE)
        }

#if(is(el, "Symbol") && el$name == "table") browser()
           
        if(is(el, "Symbol") && is(el$parent, "Call") && is(el$parent$fn, "Symbol") &&
           el$parent$fn$name %in% ApplyFunNames && !identical(el, el$parent$fn))
        {
                m = match.call(get(el$parent$fn$name), to_r(el$parent))
                if(is.name(m$FUN) && m$FUN == el$name) #XXX could be ns::foo
                    # Also, could be the same symbol in another argument other than FUN!
                    addFuns(as.character(el$name))
                else
                    addVar(el$name)
                
        } else if(is(el, "Symbol") && !(is(el$parent, "Call") && identical(el, el$parent$fn))) {
            addVar(el$name)
        } else if(is(el, "Call")) {
            if(is(el$fn, "Call")) 
                return(TRUE)

#browser()            
            if(el$fn$name %in% c("::", ":::")) {

                    # And don't process these elements below
                    # Avoid recursion from this point on, but then have to process the args.
                    addFuns(rstatic:::deparse_string(to_r(el)))
                    lapply(el$args, process)
                    return(FALSE)
            } else if(FALSE && el$fn$name %in% c("sapply", "lapply", "vapply", "apply", "mapply")) {
                addFuns(el$fn$name)  # the sapply/....
                m = match.call(get(el$fn$name), to_r(el))
                if(is.name(m$FUN)) #XXX could be ns::foo
                   addFuns(as.character(m$FUN))
                # Now we need to avoid processing this FUN element again.

            } else {
               addFuns(el$fn$name)
           }
        }

        TRUE
    }

    addVar = function(ids) {
        if(!(ids %in% c(params, localVars)))
           vars <<- c(vars, ids)
    }

    addFuns = function(ids)
        funs <<- c(funs, ids)
    
    addLocalVar = function(name)
        localVars <<- c(localVars, name)

    list(addVar = addVar,
         addFuns = addFuns,
         process = process,
         addParams = function(ids) params <<- c(params, ids),
         result = function(merge = TRUE) {
             if(merge)
                c(funs, vars)
             else
                list(functions = funs, vars = vars)
         })
}

findGlobals =
function(fun, merge = TRUE, ast = to_ast(fun), existingVars = character())    
{
    col = genCollector(existingVars)
    col$addParams(names(ast$params))

    sapply(ast$params, col$process)
    astTraverse(ast$body, col$process)

    col$result(merge)
}
