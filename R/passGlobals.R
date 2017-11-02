#
# The purpose of this is to take a collection of functions
# the reference non-local/global variables and to modify the
# functions to take these as parameters.  Then we update the
# calls to these functions to add these global variables to the calls.
# Then we iterate again to see if the functions that contain the newly modified
# calls need to be passed these global variables. If so, we add these variables
# as parameters. Then we iterate again.
#
#

do =
function(..., .funs = list(...))
{
  if(length(names(.funs)) == 0) {
      k = sys.call()
      syms = k[-1]
      nm = sapply(syms, is.name)
      names(.funs)[nm] = sapply(syms[nm], as.character)
  }
# Handle names if .funs is passed as .funs = list(f, g, main)  

  g = lapply(.funs, function(f) codetools::findGlobals(f, FALSE))
  gvars = lapply(g, function(x) x$var)
  hasNonLocals = sapply(gvars, length) > 0
  .funs[hasNonLocals] = mapply(addParams, .funs[hasNonLocals], gvars[hasNonLocals])

  updatedFuns = .funs[hasNonLocals]


  calls = sapply(g, function(f) any(unlist(f) %in% names(updatedFuns)))
  if(any(calls)) {
      .funs[calls] = lapply(.funs[calls], passGlobals, gvars[hasNonLocals])
      tmp = do(.funs = .funs[calls])
      .funs[names(tmp)] = tmp
  
      updatedFuns = append(updatedFuns, .funs[calls])
  }
  updatedFuns
}

passGlobals =
function(fun, gVarsByFun)    
{
    ofun = fun
    
    ast = to_ast(fun)
    astTraverse(ast, updateCallsFun(gVarsByFun))
    
    fun = eval(to_r(ast))
    environment(fun) = environment(ofun)
    fun
}

updateCallsFun =
    #
    # returns a function that will update a Call object (in the rstatic
    # AST node tree) which is a call to one of the functions that requires
    # global variables to be passed to it.
    #
function(gVarsByFun)
{
    function(node) {

        if(is(node, "Call") && node$fn$name %in% names(gVarsByFun)) {
           extra = gVarsByFun[[ node$fn$name ]]
           # We may want to add .x = x rather than just x by position
           # but we need to know if the . was prepended to the variable name
           # or more generally what parameter each global corresponds to
           node$args = append(node$args, lapply(extra, Symbol$new))
        }
    }
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
function(fun, varNames, addDot = TRUE)
{
    ofun = fun    
    newNames = paste0(".", varNames)
    fun = changeParamName(fun, varNames, newNames)
      # to_r(function() ...)  returns a call object for better or worse!
      # So need to evaluate that and then set the environment
    fun = eval(fun)
    environment(fun) = environment(ofun)

    for(i in seq(along = varNames)) 
        fun = addGlobalParam(fun, newNames[i], as.symbol(varNames[i]))

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
function(fun, param, default, asIs = inherits(default, "AsIs"))
{
    formals(fun)[[param]]
    z = alist(x=)
    names(z) = param
    formals(fun) = c(formals(fun), z)
    if(!missing(default))
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
function(fun, origName, newName = names(origName))
{
    ast = to_ast(fun)
    map = structure(origName, names = newName)
    astTraverse(ast, renameVarFun(map))
    to_r(ast)
}

renameVarFun =
    #
    # Returns a function that knows to change a Symbol (in the AST)
    # to a new name based on the name-value pairs in the parameter map.
    #
function(map)
{
    function(node) {
        if(is(node, "Symbol")) {
            i = match(node$name, map)
            if(!is.na(i)) {
                node$set_basename(names(map)[i])
            }
        }
    }
}
