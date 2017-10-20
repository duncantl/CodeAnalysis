library(rstatic)

genCollector =
function(...)
{
    vars = character()
    funs = character()

    process = function(el) {
        if(is(el, "Symbol") && !is(el$parent, "Call"))
            addParams(el$name)
        else if(is(el, "Call")) {
            if(is(el$fn, "Call")) 
                return(TRUE)

            
            if(el$fn$name %in% c("::", ":::")) {

                    # And don't process these elements below
                    # Avoid recursion from this point on, but then have to process the args.
                    addFuns(rstatic:::deparse_string(to_r(el)))
                    lapply(el$args, process)
                    return(FALSE)
               
            } else if(el$fn$name %in% c("sapply", "lapply", "vapply", "apply", "mapply")) {
                m = match.call(get(el$fn$name), to_r(el))
                if(is.name(m$FUN))
                   addFuns(as.character(m$FUN))
           } else
               addFuns(el$fn$name)
        }

        TRUE
    }

    addParams = function(ids)
        vars <<- c(vars, ids)

    addFuns = function(ids)
        funs <<- c(funs, ids)    

    list(addParams = addParams,
         addFuns = addFuns,
         process = process,
         result = function(merge = TRUE) {
             if(merge)
                table(c(funs, vars))
             else
                list(functions = table(funs), vars = table(vars))
         })
}

findGlobals =
function(fun, merge = TRUE, ast = to_ast(fun))    
{
    col = genCollector()
    col$addParams(names(ast$params))

    sapply(ast$params, col$process)
    astTraverse(ast$body, col$process)

    col$result(merge)
}
