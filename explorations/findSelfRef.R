# Functions which have parameters with default values that reference local variables
# created in the body of the function.
# This is fine if the local variables are created before the parameter is used.
# However, if the parameter is "forced" before the local variable is assigned, we have problems.
#
# getGlobals() detects this.
#

if(FALSE) {

    # Circular reference.
    eg = function(x = length(y), y = sum(x), z = x + y) {
        x
    }
    
    fun = function(x, var, colors = rainbow(length(levels(fac)))) {
            if(length(colors) == 0)
                colors = "black"
            
            fac = factor(x[[var]])
            plot(x$y, col = colors[fac])
    }
    
    fun2 = function(x, mu = n) {
        
            if(any(is.an(mu)))
                mu = mu[!is.na(mu)]

            n = length(x)
            rnorm(x, mu)
        }
}

if(FALSE) {
    # find functions which have one or more parameters that have a default value
    # that depend on a variable other than the other parameters.
    # And then see if these are local variables, rather than global variables,
    # and then if the local variables are defined before the parameter is used.
    #
    pkgs = list.files(file.path(R.home(), "library"), full = TRUE)
    pkgs = pkgs[ file.info(pkgs)$isdir ]
    pkgs = pkgs[ file.exists(file.path(pkgs, "R")) ]
    pkgNames = setdiff(basename(pkgs), "tcltk")

    tmp = lapply(pkgNames, function(p) {
                             if(!require(p, character.only = TRUE))
                                 return(list())
                             fns = getFunctionDefs(getNamespace(p))
                             tmp = lapply(fns, findCircularParamValues)
                             tmp[sapply(tmp, length) > 0]
                     })

    names(tmp) = pkgNames
    tmp2 = unlist(tmp, recursive = FALSE)
    names(tmp2) = paste(rep(pkgNames, sapply(tmp, length)), unlist(sapply(tmp, names)), sep = "::")
    w = sapply(tmp2, length) > 1
    
    tmp = lapply(pkgNames, function(p) {
                             if(!require(p, character.only = TRUE))
                                 return(list())
                             fns = getFunctionDefs(getNamespace(p))
                             fns[sapply(fns, hasParamUsingNonParams)]
    })


    # For these functions, get the local variables that are used in the
    # parameters and the non-local variables used in the parameters.
    tmp2 = lapply(unlist(tmp, recursive = FALSE),
                  function(f) {
                      p = getParamsUsingNonParams(f)
                      g = getGlobals(f)
                      list(local = intersect(unlist(p), g$localVariables),
                           outside = intersect(unlist(p), g$variables))
                  })
}


findCircularParamValues =
function(fun, g = getParamGraph(fun))
{
    
}

getParamGraph =     
function(fun)
{
    params = formals(fun)
    hasDefault = !sapply(params, isSymbol, "")

    if(!any(hasDefault))
        return(data.frame(param = character(), uses = character()))

    # Build a graph of what depends on what and then see if there is a cycle.
    # Could be conditional.

    paramNames = names(params)
    ans = lapply(params, function(p) intersect(getAllSymbols(p), paramNames))
    ans = ans[ sapply(ans, length) > 0 ]    
    data.frame(param = rep(names(ans), sapply(ans, length)),
               uses = unlist(ans))
}

###########


hasParamUsingNonParams =
function(fun) 
  length(getParamsUsingNonParams(fun)) > 0


getParamsUsingNonParams =
    #
    # For parameters that have default values, find which
    # symbols/variables they use that are not other parameters.
    # This doesn't include functions that are called, just variables.
    # The idea is to be able to find default value expressions that
    # use either global variables or local variables defined within the function.
    # Then we care about whether those local variables are defined before the
    # default value is "forced". getGlobals() detects this.
    #
function(fun)    
{
    params = formals(fun)
    hasDefault = !sapply(params, isSymbol, "")
    if(!any(hasDefault))
        return(list())

    tmp = lapply(params[hasDefault], defaultValueUsesNonParams, names(params))
    tmp[sapply(tmp, length) > 0]
}

defaultValueUsesNonParams =
function(expr, params)
{
    if(isLiteral(expr))
        return(character())
    
    if(is.name(expr))
       return(setdiff(as.character(expr), params))

    gvars = getGlobals(expr)$variables
    setdiff(gvars, params)
}




if(FALSE) 
gfindGlobals =
function(e)
{
    f = function() 1
    environment(f) = globalenv()
    body(f) = e
    print(f)
    findGlobals(f)
}

