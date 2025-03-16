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
# source("Topics/globalsRewrite/example/simple.R")
# z = mkGlobalsLocal(f, g, main)

mkGlobalsLocal =
function(..., .funs = list(...), .addDefaults = rep(TRUE, length(.funs)))
{
  if(missing(.funs) && length(names(.funs)) == 0) {
        # Caller gave functions via ... but no names.
      k = sys.call()
      syms = k[-1]
  } else if(length(names(.funs)) == 0) {
      # Handle names if .funs is passed as .funs = list(f, g, main)        
      k = sys.call()
      syms = k[[2]][-1]
  }
  nm = sapply(syms, is.name)
  names(.funs)[nm] = sapply(syms[nm], as.character)    

  # ¿ Why not getGlobals() ?
  g = lapply(.funs, codetools::findGlobals, FALSE)
  gvars = lapply(g, `[[`, "variables")
  hasNonLocals = sapply(gvars, length) > 0
  .funs[hasNonLocals] = mapply(addParams, .funs[hasNonLocals], gvars[hasNonLocals])

  updatedFuns = .funs[hasNonLocals]

  # See which functions call any of these updated functions.
  # We will have to change their calls to these updated functions.
  calls = sapply(g, function(f) any(unlist(f) %in% names(updatedFuns)))
  if(any(calls)) {
      # Add arguments to the calls to these updated functions
      .funs[calls] = lapply(.funs[calls], passGlobals, gvars[hasNonLocals])
      tmp = mkGlobalsLocal(.funs = .funs[calls])
      .funs[names(tmp)] = tmp
  
      updatedFuns = append(updatedFuns, .funs[calls])
  }
  
  updatedFuns
}

# rstatic
if(FALSE)
passGlobals =
    #
    # Add additional arguments to calls to any of the functions
    # named in gVarsByFun
    #
function(fun, gVarsByFun)    
{
    ofun = fun
    
    ast = to_ast(fun)
    replace_nodes(ast, updateCallsFun(gVarsByFun), in_place = TRUE)    
    #astTraverse(ast, updateCallsFun(gVarsByFun))
    
    fun = eval(as_language(ast))
    environment(fun) = environment(ofun)
    fun
}

# rstatic
if(FALSE)
updateCallsFun =
    #
    # returns a function that will update a Call object (in the rstatic
    # AST node tree) which is a call to one of the functions that requires
    # global variables to be passed to it.
    #
function(gVarsByFun)
{
    function(node) {

        if(is(node, "Call") && is(node$fn, "Symbol")&& node$fn$value %in% names(gVarsByFun)) {
           extra = gVarsByFun[[ node$fn$value ]]
           #XXX We may want to add .x = x rather than just x by position
           # but we need to know if the . was prepended to the variable name
           # or more generally what parameter each global corresponds to
           id = names(node$args$contents)
           if(length(id) != length(node$args$contents))
               id = character(length(node$args$contents))
           node$args$contents = append(node$args$contents, lapply(extra, Symbol$new))
           names(node$args$contents) = c(id, paste0(".", extra))
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
    newNames = if(addDot) paste0(".", varNames) else varNames
    fun = changeParamName(fun, varNames, newNames)
      # as_language(function() ...)  returns a call object for better or worse!
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


# rstatic
if(FALSE)
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
    replace_nodes(ast, renameVarFun(map), in_place = TRUE)
    # astTraverse(ast, renameVarFun(map))
   
    as_language(ast)
}

# rstatic
if(FALSE)
renameVarFun =
    #
    # Returns a function that knows to change a Symbol (in the AST)
    # to a new name based on the name-value pairs in the parameter map.
    # Symbol with name values not in the map remain unchanged.
    #
function(map)
{
    function(node) {
        if(is(node, "Symbol")) {
            i = match(node$value, map)
            if(!is.na(i)) {
                #node$set_basename(names(map)[i])
                node$value = names(map)[i]
            }
        }
        node
    }
}
