getGlobals =
    #
    # This is like codetools::findGlobals() but can be made aware of
    # function calls which we want to ignore because they may refer to 
    # global variables that are in an entirely different scope, e.g. R.
    # This is for compiling callbacks to R and identifying variables that
    # are to be resolved in R.
    #
    # This is not the same as findGlobals (yet!), and is probably not as comprehensive
    # but also finds some errors, i.e. finds use of variables before they are defined/assigned.
    #
    # We didn't use the code walker mechanism in codetools as it doesn't appear to give us a means
    # to push and pop function calls. But this could be just that I haven't wrapped my head around it.
    #
    # Todo:  process nested function definitions and determine their local variables
    #
    #
    #
    # When run on compileFunction(), we find ir and nenv as globals.
    # nenv is real as it used before it is defined.
    #  ir appears as a global because its is referenced in a function definition
    #  not a call.
    #
    #
    # skip  is for the names of functions for which we are to ignore calls to these
    #
    #
    # To find functions that have a FUN argument
    #
    # bvars = ls("package:base")
    # hasFun = structure(sapply(bvars, function(x) { f = get(x, "package:base"); if(is.function(f)) "FUN" %in% names(formals(f)) else FALSE}), names = bvars)
    #  bvars[hasFun]
    #
function(f, expressionsFor = character(), .ignoreDefaultArgs = FALSE, 
         skip = c(".R", ".typeInfo", ".signature", ".pragma"),
         .debug = TRUE, .assert = TRUE,
         localVars = character())
{

  if(is.logical(.debug))
      .debug = if(.debug) ".debug" else character()
  if(is.logical(.assert))
      .assert = if(.assert) ".assert" else character()  

  skip = c(skip, .debug, .assert)
    
  vars = character()
  funs = character()
  varsByFun = list()

  curFuns = character()
  expressions = list()

  subFunInfo = list()

  skippedExpressions = list()
  
  addFunName = function(id) {
                  if(!(as.character(id) %in% c(c("for", "if", "{", "while"), skip)))
                     funs <<- c(funs, as.character(id))
               }

  procIndirectFunCall = function(e, funName = as.character(e[[1]]), def = get(funName)) {
      e2 = match.call(def, e)
      i = switch(funName,
             do.call = match("what", names(e2)),
             aggregate = if(length(e2) > 3) 4 else NA, # taking some liberties here matching not by name but known position for methods. See ?aggregate.
             match("FUN", names(e2)))
      if(!is.na(i) && (is.character(e2[[i]]) || is.name(e2[[i]]))) {
          if(!(as.character(e2[[i]]) %in% localVars))
              addFunName(e2[[i]])
          e2[-i]
      } else
          e2
  }
  
  fun = function(e, w) {
      popFuns = FALSE
      if(is.name(e) && as.character(e) == "")  # typically a formal argument that has no default value.
          return(FALSE)

      
      if(is(e, "if")) {
          if(e[[2]] == FALSE) {
              if(length(e) == 3)
                 return(FALSE)
              else
                 e = e[[4]]
          } else if(e[[2]] == TRUE) {
              e = e[[3]]
          } else
             # e = e[-1]
            return(lapply(e[-1], fun, w))

      }  # fall through

      if(class(e) == "for") 
          localVars <<- c(localVars, as.character(e[[2]]))

          
      if(is.call(e)) {
           if(is.call(e[[1]]))  # e.g. x$bob()
               return(lapply(e, fun,  w))

          
           funName = as.character(e[[1]])
if(funName == 'names') browser()
           if(funName == "function") {
              subFunInfo[[length(subFunInfo)+1L]] <<- getGlobals(e, expressionsFor, skip = skip, .ignoreDefaultArgs = .ignoreDefaultArgs)
             return(TRUE)
          } else if(funName %in% c('<-', '=')) {
              fun(e[[3]], fun)
              e = e[-3]
              if(is.name(e[[2]]))
                  localVars <<- c(localVars, as.character(e[[2]]))
           } else {
              if(funName %in% skip) {
                 i = length(skippedExpressions) + 1L
                 skippedExpressions[[ i ]] <<- e
                 names(skippedExpressions)[i] <<- funName
                 return(TRUE)
              }
              
              if(length(curFuns))
                 sapply(curFuns, function(id)
                                    varsByFun[[id]] <<- c(varsByFun[[id]], funName))
              curFuns <<- c(curFuns, funName)
              if(!(funName %in% localVars))
                 addFunName(funName) # funs <<- c(funs, funName)
             
              popFuns = TRUE

             if(funName %in% expressionsFor)
                 expressions[[ funName ]] <<- c(expressions[[ funName ]], e)
          }

          els = as.list(e)[-1]
          if(funName == "$") # remove the literal name
              els = els[-2]
          else if (funName %in% c("apply", "eapply", "sweep", "sapply", "lapply", "vapply", "mapply", "tapply", "by", "aggregate", "do.call", "match.fun", "kronecker", "outer", "sweep")) {
                els = procIndirectFunCall(e, funName)
          }
##          else if(funName %in% c("sapply", "lapply") && is.name(e[[3]])) {
##             addFunName(as.character(e[[3]])) #XXX Also need to add it to the varsByFuns
##             els = els[-2]
##          } else if(funName %in% c("mapply") && is.name(e[[2]])) {
##             addFunName(as.character(e[[2]])) #XXX Also need to add it to the varsByFuns
##             els = els[-2]
##          } else if(funName %in% c("do.call") && (is.name(e[[2]]) || is.character(e[[2]]))) {
##             addFunName(as.character(e[[2]])) #XXX Also need to add it to the varsByFuns
##             els = els[-2]
##          }
           
          lapply(els, fun, w)
          if(popFuns)
              curFuns <<- curFuns[- length(curFuns)]
      } else if(is.name(e) && !(as.character(e) %in% localVars)) {
             name = as.character(e)
             if(!(name %in% localVars) && length(curFuns) && any(expressionsFor %in% curFuns)) {
             } else
                vars <<- c(vars, name)
             
             if(length(curFuns))
                 sapply(curFuns, function(id)
                                    varsByFun[[id]] <<- c(varsByFun[[id]], name))
        } else
          lapply(as.list(e)[-1], fun, w)
  }

  if(is.call(f) && as.character(f[[1]]) == "function") 
     f = eval(f, globalenv())

  if(typeof(f) == "closure") {
      localVars = names(formals(f))
      if(!.ignoreDefaultArgs)
         lapply(formals(f), fun, fun)
      f = body(f)
  } 
  
  fun(f, fun)

  varsByFun = varsByFun[ setdiff(names(varsByFun), c("for", "if", "{"))]

    #XXX This doesn't catch the case that a function is defined and called before the variable it references in the parent function
    # is defined.

  gvs = unlist(sapply(subFunInfo, `[[`, "variables"))
  i = match(gvs, localVars)
  vars = c(vars, gvs[is.na(i)])
  list(localVariables = localVars,
       variables = vars,
       functions = funs,
       variablesByFun = lapply(varsByFun, table),
       expressions = expressions,
       subFunctions = subFunInfo,
       skippedExpressions = skippedExpressions)
}





findCallsParam =
    #
    # Find any direct calls to a parameter, e.g.
    #  optim() may call its fn and gr
    #
    # Doesn't find indirect calls such as function(x, f) sapply(x, f).
    # Get those with getGlobals()
    #
function(fun)
{
   params = names(formals(fun))
   funa = to_ast(fun)
   v = find_nodes(funa, function(x) is(x, "Call") && is(x$fn, "Symbol") && x$fn$value %in% params)
   vapply(v, function(x) x$fn$value, character(1))
}
