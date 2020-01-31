getGlobals =
    #
    # This is like codetools::findGlobals() but can be made aware of
    # function calls which we want to ignore because they may refer to 
    # global variables that are in an entirely different scope, e.g. R. (?)
    # This is for compiling callbacks to R and identifying variables that
    # are to be resolved in R.
    #
    # This is not the same as findGlobals (yet!), and is probably not as comprehensive
    # but also finds some errors, i.e. finds use of variables before they are defined/assigned.
    #
    #  Actually it is more correct and comprehensive than findGlobals.
    #  It recognizes when variables are defined locally and identifies uses of it before that as globals.
    # 
    # It also attempts to deal with 
    #
    # We didn't use the code walker mechanism in codetools as it doesn't appear to give us a relatively simple means
    # to push and pop function calls. But this could be just that I haven't wrapped my head around it.
    # But also, it is a slightly akward interface and undocumented.
    #
    # Todo:  process nested function definitions and determine their local variables
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
    # To find functions that have a FUN argument which is likely to be called directly, we can use
    #
    # bvars = ls("package:base")
    # hasFun = structure(sapply(bvars, function(x) { f = get(x, "package:base"); if(is.function(f)) "FUN" %in% names(formals(f)) else FALSE}), names = bvars)
    #  bvars[hasFun]
    #
    #
    # Doesn't identify do.call, optim, etc.  But for optim, we have findCallsParam().
    #
function(f, expressionsFor = character(), .ignoreDefaultArgs = FALSE, 
         skip = c(".R", ".typeInfo", ".signature", ".pragma"),
         .debug = TRUE, .assert = TRUE,
         localVars = character(), mergeSubFunGlobals = TRUE)
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
      # remove ... from e as match.call
      #   ... used in a situation where it does not exist
      w = sapply(e, function(x) is.name(x) && x == "...")
      if(any(w))
          e = e[!w]
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

  curAssignName = character()
  
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
#if(funName == 'foo') browser()
           if(funName == "function") {
               #XXX Should be able to get the name of this if it is available.
              subFunInfo[[length(subFunInfo)+1L]] <<- getGlobals(e, expressionsFor, skip = skip, .ignoreDefaultArgs = .ignoreDefaultArgs)
              if(length(curAssignName) > 0)
                  names(subFunInfo)[length(subFunInfo)] = curAssignName[1]
              return(TRUE)
          } else if(funName %in% c('<-', '=')) {
              if(is.name(e[[2]]))
                  curAssignName <<- c(as.character(e[[2]]), curAssignName)
              
              fun(e[[3]], fun)
              e = e[-3]
              if(is.name(e[[2]]))              
                  curAssignName = curAssignName[-1]
              
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

           
          lapply(els, fun, w)
          if(popFuns)
              curFuns <<- curFuns[- length(curFuns)]
       } else if(is.name(e)) {
           # with the changes for lazily dealing with default values of parameters, we now
           # have all symbols coming through this, even as name of function in a call
           # e.g.,   sapply(x, f)  will have sapply come through this.
           # So have to avoid adding it to the vars variable.

           # This needs some comparison with origina and probably needs changes to the logic.
           
          name = as.character(e)
          if(!(name %in% localVars) && name %in% names(params) && !defaultValuesProcessed[name]) {
              defaultValuesProcessed[name] <<- TRUE
              fun(params[[name]], fun)
              localVars <<- c(localVars, name)
          }

          if(!(name %in% localVars)) {
             if(name %in% funs || (!(name %in% localVars) && length(curFuns) && any(expressionsFor %in% curFuns))) {
             } else {
#                 browser()
                 vars <<- c(vars, name)
             }

             
             if(length(curFuns))
                 sapply(curFuns, function(id)
                                    varsByFun[[id]] <<- c(varsByFun[[id]], name))
          }
       } else if(typeof(e) == "externalptr") {
           
       }  else
          lapply(as.list(e)[-1], fun, w)
  }

  if(is.call(f) && as.character(f[[1]]) == "function") 
     f = eval(f, globalenv())


  if(is.function(f)) {
    params = formals(f)
    defaultValuesProcessed = structure(rep(FALSE, length(params)), names = names(params))
  } else {
    params = list()
    defaultValuesProcessed = logical()
  }
  
  
  if(typeof(f) == "closure") {

      if(!.ignoreDefaultArgs) {
            # process the parameters that have no default value.
         localVars = names(params)[sapply(params, function(x) is.name(x) && x == "")]
         # lapply(params, fun, fun)
         f = body(f)
     }
  } 
#  origFun = body(f)

  fun(f, fun)

  if(any(!defaultValuesProcessed))
      lapply(params[!defaultValuesProcessed], fun, fun)
  
  varsByFun = varsByFun[ setdiff(names(varsByFun), c("for", "if", "{"))]

    #XXX This doesn't catch the case that a function is defined and called before the variable it references in the parent function
    # is defined.

  gvs = unlist(sapply(subFunInfo, `[[`, "variables"))
  i = match(gvs, localVars)
  vars = c(vars, gvs[is.na(i)])

  ans = structure(list(localVariables = localVars,
                 variables = vars,
                 functions = funs,
                 variablesByFun = lapply(varsByFun, table),
                 expressions = expressions,
                 subFunctions = subFunInfo,
                 skippedExpressions = skippedExpressions),
      class = "GlobalUses")

  if(mergeSubFunGlobals) 
      ans$functions = getGlobalFunctions(ans, TRUE, TRUE)

  ans

}


getGlobalFunctions =
function(x, ...)   
    UseMethod("getGlobalFunctions")

getGlobalFunctions.GlobalUses =
function(x, mergeSubFuns = FALSE, asNames = TRUE, ...)
{    
    ans = x$functions
    if(mergeSubFuns) {
        # This isn't quite right as we might define a function in the top-level function
        # and so it is not global, but intermediate. But then it would be available to those
        # functions even after they are defined
        # e.g.   function() { foo = function(x) bar(x); bar = function(y) y+1; ...}
        # We need to pass the names of the existing functions down to getGlobals when we process a new function.
        # But again, are available after the definition of that function.
        #
        
        # Need to exclude the functions that are defined within the top-level function
        # 
        tmp = unlist(lapply(x$subFunctions, `[[`, "functions"))
        
        ans = c(ans, tmp[ !(tmp %in% x$localVariables)] )
   }

    if(asNames)
        ans
    else
        table(ans)
}


#############################

