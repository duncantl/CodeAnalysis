#
# The purpose of this is to take a collection of functions
# that reference non-local/global variables and to modify the
# functions to take these as parameters.  Then we update the
# calls to these functions to add these global variables to the calls.
# Then we iterate again to see if the functions that contain the newly modified
# calls need to be passed these global variables. If so, we add these variables
# as parameters. Then we iterate again.
#
# This doesn't handle functions that are called via *apply.
# We could wrap those in closures. Or get the default
# values to be the global variables.
#
# @example
# source("explorations/globalsRewrite/example/simple.R")
# z = mkGlobalsLocal(f, g, main)
#
# lapply(z, \(x) getGlobals(x)$variables)
#

mkGlobalsLocal =
function(..., .funs = list(...), .addDefaults = rep(TRUE, length(.funs)), addDot = FALSE, addDefault = FALSE)
{
    if(! (length(names(.funs)) > 0 && all(names(.funs) != ""))) {
        # Fix the names on .funs as they can be supplied in different ways for convenience.
        k = sys.call()
        getNames = function(k) {
            # if no named argument in the call, match.call won't put the names on.
            k2 = as.list(match.call(mkGlobalsLocal, k)[-1])
            if(length(names(k2) > 0))
               k2[  !( names(k2) %in%  names(formals(mkGlobalsLocal))) ]
            else
               k2
        }

        # XXX merge these later.
        if(missing(.funs) && length(names(.funs)) == 0) {
            # Caller gave functions via ... but no names.
            syms = getNames(k)
        } else if(length(names(.funs)) == 0) {
            # Handle names if .funs is passed as .funs = list(f, g, main)
            syms = getNames(k)      
        } 
    
        nm = sapply(syms, is.name)
        names(.funs)[nm] = sapply(syms[nm], as.character)    
    }
    
    # Why not getGlobals() ?
    g = lapply(.funs, codetools::findGlobals, FALSE)
    gvars = lapply(g, `[[`, "variables")
    hasNonLocals = sapply(gvars, length) > 0
    .funs[hasNonLocals] = mapply(addParams,
                                 .funs[hasNonLocals],
                                 gvars[hasNonLocals],
                                 MoreArgs = list(addDot = addDot, addDefault = addDefault))

    updatedFuns = .funs[hasNonLocals]

    # See which functions call any of these updated functions.
    # We will have to change their calls to these updated functions.

    #  should probably be f$functions and we could do that if we use getGlobals as it will find indirect calls.
    calls = sapply(g, function(f) any(unlist(f) %in% names(updatedFuns)))

    if(any(calls)) {
        #XXX go through the different cases here.
        # 1. moved nested functions out and the variables they need are in the host function.
        # 2. the host function needs the non-local variables also since they are truly global
        #    variables and not defined in any of the functions.
        #
        # Add arguments to the calls to these updated functions
        .funs[calls] = lapply(.funs[calls], passGlobals, gvars[hasNonLocals])
        tmp = mkGlobalsLocal(.funs = .funs[calls])
        .funs[names(tmp)] = tmp
        
        updatedFuns = append(updatedFuns, .funs[calls])
    }
  
    updatedFuns
}


passGlobals =
    #
    # Add additional arguments to calls to any of the functions
    # named in gVarsByFun
    #
    # We should have an optional argument that specifies what variable/call corresponds
    # to these parameters.
    # If NA then add them as formals to fun as well.
    #
    # XXXX need to either have caller fixup gVarsByFun or do it here.
    # And if just get naes of additional arguments, need to create
    # new formals in fun and then map those
    #
    # What if, as in our example (simple.R), we have f and g and both take an alpha argument
    # and main calls f and g.
    # For now, assume same alpha since it was global.
    #
    # return fun
    #
function(fun, gVarsByFun)
{
    gVarsByFun = lapply(gVarsByFun, function(x) { if(length(names(x)) == 0) names(x) = x; x})
    params = unique(unlist(gVarsByFun))
    w = setdiff(params, names(formals(fun)))
    if(length(w)) 
        fun = addParams(fun, w, FALSE, FALSE)
    
    rw = genAddArgsToCalls(gVarsByFun)
    w = mkConstPropWalker(rw, FALSE)
    walkCode(fun, w)    
}

addParams =
    #
    # Given a function which currently references
    # non-local/global variables, change the variable names
    # to have a . prepended and then add these as parameters
    # and a default value which is the name of the original global
    # variable.
    # So, e.g.,
    #   function(x) x + beta
    # becomes
    #   function(x, .beta = beta) x + .beta
    #
    # XXX this or probably the caller needs to ensure that the variables
    #
function(fun, varNames, addDot = TRUE, addDefault = addDot)
{

    newNames = if(addDot)
                   paste0(".", varNames)
               else
                   varNames
    if(addDot) {
        ofun = fun
        #XXX fix changeParamName
        fun = changeParamName(fun, varNames, newNames)
        # as_language(function() ...)  returns a call object for better or worse!
        # So need to evaluate that and then set the environment
        fun = eval(fun)
        environment(fun) = environment(ofun)
    }

    for(i in seq(along = varNames)) 
        fun = addGlobalParam(fun, newNames[i], as.symbol(varNames[i]), noDefault = !addDefault)

    fun
}





addGlobalParam =
    #
    # Add a new parameter to a function with the specified name
    # and if default is provided add it as the default value
    # This default can be evaluated or not in this call.
    #
    # Tests
    # f = function(x) {}
    # addGlobalParam(f, "bob")
    # addGlobalParam(f, "bob", 1)
    # addGlobalParam(f, "bob", as.symbol("GlobalBob"))
    #
    # addGlobalParam(f, "bob", 1 + 3)
    # addGlobalParam(f, "bob", 1 + x, TRUE)
    #
    # If you want to add an expression
    #
function(fun, param, default, asIs = inherits(default, "AsIs"), noDefault = missing(default))
{
    formals(fun)[[param]]
    z = alist(x=)
    names(z) = param
    formals(fun) = c(formals(fun), z)
    if(!noDefault) # !missing(default))
        formals(fun)[[param]] = if(asIs) substitute(default) else default
    
    fun
}


changeParamName =
    #
    # Rewrite the body of a function to change the name of one or more
    # variables to a new name, e.g.
    #
    # f = function(x, b) { x + b + 1}
    # changeParamName(f, c("x", "b"), c(".x", ".b"))
    #
    # Which then becomes
    # function(x, .b) { x + .b + 1}
    #
    # This new version reverses the names-newNames in origName. Not a big deal
    #
function(fun, origName, newName = names(origName))
{
    #XXX see explorations/modifyCode/propagate.R.  The function names will change.
    rw = genRewriteVars(structure(newName, names = origName))
    w = mkConstPropWalker(rw, FALSE)
    walkCode(fun, w)    
}



