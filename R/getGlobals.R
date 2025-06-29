getGlobals =
    #
    # This is like codetools::findGlobals() but can be made aware of
    # function calls which we want to ignore because they may refer to 
    # global variables that are in an entirely different scope, e.g. R. (?)
    # This is for compiling callbacks to R and identifying variables that
    # are to be resolved in R.
    #
    # This is not the same as findGlobals. 
    # It finds some aspects findGlobals doesn't handle
    #  e.g. finds use of variables before they are defined/assigned
    #         i.e., defined locally but used before that. 
    #       functions such lapply/sapply/... and do.call that often refer to the function they call via variable.
    #         findGlobals() identifies order and foo as global variables, rather than non-local functions.
    #          lapply(x, order)
    #          do.call(foo, args)
    #         
    # 
    # It also attempts to deal with ....??????
    #
    # We didn't use the code walker mechanism in codetools as it doesn't appear to give us a relatively simple means
    # to push and pop function calls. But this could be just that I haven't wrapped my head around it.
    # But also, it is a slightly awkward interface, undocumented and states it is not to be used/relied upon - after 10-15 years!
    #
    # Todo:  process nested function definitions and determine their local variables
    #
    #    # XXX Doesn't include [[<-, [<-, functions.
    #    # Handle default values and references to local variables and check whether the default value
    #       is needed before those local variables are defined
    #
    #  $<- implemented. Process only the second element  x$y <- value, so process x since y is verbatim.
    #   The RHS is handled elsewhere.
    #    Added 
    #
    # When run on compileFunction(), we find ir and nenv as globals.
    # nenv is real as it used before it is defined.
    #  ir appears as a global because its is referenced in a function definition
    #  not a call.
    #
    # skip  is for the names of functions for which we are to ignore calls to these
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
    #
    #
    # Intentionally doesn't include for, if, while, { as functions which findGlobals() does.
    #
function(f, expressionsFor = character(), .ignoreDefaultArgs = FALSE,
              #XX we probably want to process the arguments within .Internal except the first which is the call.
         skip = c(".R", ".typeInfo", ".signature", ".pragma", ".Internal", ".Primitive"), 
         .debug = TRUE, .assert = TRUE,
         localVars = character(), mergeSubFunGlobals = TRUE,
         old = TRUE, # remove old when we are sure it worksa
         indirectCallFunctions = names(getIndirectCallFunList()),
         handleTextConnections = TRUE,
         availableVars = character(),
         funDefs = list()
         ) 
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

    # top-level function being analyzed
    topFun = NULL
    topFun = if(is.function(f)) f


    addFunName = function(id) {
                  if(!(as.character(id) %in% c(c("for", "if", "{", "while"), skip)))
                     funs <<- c(funs, as.character(id))
               }

    # this for handling calls which we know will call one of their arguments, e.g., lapply(x, fun, z)
    procIndirectFunCall = function(e, funName = as.character(e[[1]]),
                                   def = getFunDef(funName, funDefs)
                                   )
    {

      # remove ... from e as match.call
      #   ... used in a situation where it does not exist
      w = sapply(e, function(x) is.name(x) && x == "...")
      if(any(w))
          e = e[!w]

      if(length(e) == 1)
          return(e)

      # XXX
      # The function def may not be globally visible but may be defined within a function.
      # e.g., sApply in tools/R/QC.R  codocClasses.
      # Wouldn't we have caught this assignment in the function. Should we pass these via an extra argument
      #  Maybe use def = list()  and look in there for the defn.
      if(is.null(def))
          return(e)
      
      e2 = match.call(def, e)

      if(length(e2) == 1 && funName %in% c("match.call", "formals")) {
#          browser()
#          warning("cannot currently determine function in empty call to ", funName, ". That uses the context of the call.")
          return(NA) # need to know the name of the function in which match.call()/formals() is being invoked.
      }
      
      
      i = switch(funName,
                 do.call = match("what", names(e2)),
                 aggregate = if(length(e2) > 3) 4 else NA, # taking some liberties here matching not by name but known position for methods. See ?aggregate.
                 Filter=,
                 Negate=,
                 Find=,
                 Position=,
                 Map=,
                 Reduce=,
                 vapply =,
                 lapply = ,
                 sapply =,                 
                 rapply =,
                 body = 2L,
                 apply = 3L,
                 by = 3L,
                 tapply = 3L,
                 match.call = 2L,
                 formals = 2L,
                 grep("fun$", names(e2), ignore.case = TRUE)) # match(c("fun", "FUN"), names(e2)))

      if(length(i) > 1) {
          warning("matched multiple arguments - using first match")
          i = i[1]
      }
      
          

      if(length(i) == 0) { # grep("fun$") not matching so FUN not in call but probably in fn defn with a default parameter
          i = grep("fun$", names(formals(def)), ignore.case = TRUE)
          if(length(i)) {
              if(is.name(formals(def)[[i]]) &&  as.character(formals(def)[[i]]) == "")
                  return(list())
              return(formals(def)[[i]])
          } else {
              warning("couldn't identify function in ", deparse(e2))
              NA
          }
          
      } else if(!is.na(i) && i <= length(e2) && (is.character(tmp <- e2[[i]]) || is.name(tmp) || isColonCall(tmp))) {
          if(!((val <- deparse(tmp)) %in% localVars))  # as.character rather than deparse()
              addFunName(val)
          e2[-i]
      } else
          e2
  }

  curAssignName = character()

    #XXX better name.
    fun = function(e, w) {

      popFuns = FALSE
      if(is.name(e) && as.character(e) == "")  # typically a formal argument that has no default value.
          return(FALSE)

      if(isCallTo(e, "%>%", indirect = FALSE)) 
         e = rewritePipeCall(e)

      
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
            return(lapply(as.list(e)[-1], fun, w))

      }  # fall through

      if(class(e) == "for") 
          localVars <<- c(localVars, as.character(e[[2]]))

      if(is.call(e)) {
          if(is.call(fn <- e[[1]])) {  # e.g. x$bob()
#              fn = e[[1]]
              if(is.name(fn[[1]]) && isColonCall(fn)) { ### (as.character(fn[[1]]) == "::" || as.character(fn[[1]]) == ":::")) {
                  addFunName(deparse(fn))
                  e = e[-1]
              }
              return(lapply(e, fun,  w))
          }

          funName = as.character(fn) # e[[1]])

          if(funName %in% skip)
              return(NULL)


          if(funName == "::" || funName == ":::") {
              addFunName(deparse(e))
              return(NULL)    # Return or keep going?  Return or will get pkg/first element of :: in variables.
          } else if(funName == "function") {
                #XXX Should be able to get the name of this if it is available.
              subFunInfo[[length(subFunInfo)+1L]] <<- getGlobals(e, expressionsFor, skip = skip,
                                                                 .ignoreDefaultArgs = .ignoreDefaultArgs,
                                                                 # here we provide the names of all the variables
                                                                 # we have seen in this function that are available
                                                                 # to the sub-function.
                                                                 # localVars =
                                                                 availableVars = unique(c(localVars, vars, if(!is.null(topFun)) names(formals(topFun)), availableVars)))
              if(length(curAssignName) > 0)
                  names(subFunInfo)[length(subFunInfo)] = curAssignName[1]
              
              return(TRUE)
          } else if(funName %in% c('<-', '=')) {
              
              if(is.name(e[[2]]))
                     # simple assignment on LHS.
                  curAssignName <<- c(as.character(e[[2]]), curAssignName)

              
              if(is.call(tmp <- e[[3]]) && is.name(tmp2 <- tmp[[1]]) &&
                   (as.character(tmp2) == "::" || as.character(tmp2) == ":::") && is.name(e[[3]][[2]]) && is.name(e[[3]][[3]])) {
                  # RHS is of the form  pkg::sym

                  vars <<- c(vars, deparse(tmp))
              } else
                  fun(tmp, fun)  # process the RHS.
              
              e = e[-3]
              if(is.name(tmp <- e[[2]])) {             
                  curAssignName = curAssignName[-1]
                  localVars <<- c(localVars, as.character(tmp))
              } else if(is.call(e[[2]])) {
                  # Put the <- in the name of the function being called.
                  # !! Need to handle pkg::fun and pkg:::fun.
                  # browser()
                  #   was as.character(e[[2]][[1]])
                  funName = paste0(deparse(e[[2]][[1]]), "<-")
                  e[[2]][[1]] = as.name(funName)
              }
              
          } else if(funName == "$<-") {
              #              e = e[[ 2 ]]
              # ?? Are there any other parts of e we need to process.
              fun(e[[2]])
              return()
          } else if(funName == "substitute" && length(e) > 2) {
              # Should we bother with matching the arguments via match.call()
              # to avoid the "unusual"
              #     substitute(env = list(a = 1), expr)
              # k = match.call(substitute, e)
              # match.call() doesn't work with substitute.
              tmp = fun(e[[3]], fun)
              return()
          } else if(funName == "quote") {
              # only one argument so skip it as it is intentionally allowed reference non-locals
              # since may not be evaluated within this function frame/environment.
              return()
          } else if(funName == "bquote") {

              k = match.call(bquote, e)

              # empty call to bquote()
              # Can be bquote(expr = ) and match.call() will reduce this to bquote()
              if(length(k) == 1)
                  return()                            

              # if the where and/or splice arguments are provided, process them.
              if(length(k) > 2)
                   lapply(k[ -c(1, 2)], fun, fun)

              # If it has a where argument, then can't statically really know
              # what variables are available within the where. So don't process the first argument.              
              if("where" %in% names(k))
                  return()


               # Otherwise, find the .() and ..() in the first argument and process those
              dot = findCallsTo(k[[2]], c(".", ".."), parse = FALSE)
              lapply(dot, fun, fun)
              return()
          } else if(funName %in% c("require", "library")) {
              # XXX Check. Assumes package is the first argument. Should do match.call()
              lapply(e[-c(1, 2)], fun, fun)
              return()
          } else {

              if(handleTextConnections && funName == "textConnection") {
                  # Special case.  If call
                  #  textConnection("bob", "w", local = TRUE)
                  # then bob becomes a local variable.
                  # Should be using textConnectionValue() rather than the variable
                  # but this is an issue with the implementation of writeable textConnection().
                  # Also note that this test requires that open = "w" and local = TRUE
                  # literally, not with variables.
                  # Use constant propogation before to see if this can update such variables.
                  k = match.call(textConnection, e)
                  if(all(c("object", "open", "local") %in% names(k)) &&
                     is.character(k$object) && k[["open"]] == "w" && isTRUE(k[["local"]]))
                      localVars <<- c(localVars, k$object)
              }              

              
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
# if(funName == "apply") browser()
          if(funName == "$") # remove the literal name
              els = els[-2]
          else if (funName %in% indirectCallFunctions || grepl("apply", funName, ignore.case = TRUE)) 
              els = procIndirectFunCall(e, funName)

          # procIndirectFunCall may not return a list.
          if(!is.symbol(els) && is.list(els))
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
              if(.ignoreDefaultArgs == FALSE) 
                  fun(params[[name]], fun)
              localVars <<- c(localVars, name)
          }

           # XXX shouldn't we put names(formals(topFun)) into localVars at the start??
           if(!(name %in% c(localVars, availableVars, if(!is.null(topFun)) names(formals(topFun))))) {
             if( name %in% funs || (!(name %in% localVars) && length(curFuns) && any(expressionsFor %in% curFuns))) {
             } else {
                 #                 browser()
                 #                 if(name  == "xml2" || name == "codetools") browser()
# XXXX
                 #  If we have this debugging step,
                 # system.time(getGlobals(funs2$.install_packages)$variables)
                 # takes 5.34 seconds. So a factor of 10
                 # Take it out, we get down to .443 seconds
#       if(name  == "utils") browser()
                 # How would we insert this and ensure we can identify it easily as a debug statement and remove it
                 # or have it as a no-op
                 vars <<- c(vars, name)
             }
             
             if(length(curFuns))
                 sapply(curFuns, function(id)
                                    varsByFun[[id]] <<- c(varsByFun[[id]], name))
         }
       } else if(typeof(e) == "externalptr") {
           
       }  else {

           if(is.function(e) && .ignoreDefaultArgs) {
               b = body(e)
               tmp = if(class(b) == "{") as.list(b)[-1] else list(b)
           } else
               tmp = as.list(e)[-1]               

           lapply(tmp, fun, w)

       }
        
  } # end of fun = function() {}

    
    # Turn a call to function into an actual function object.
  if(is.call(f) && isSymbol(f[[1]], "function"))  {
      f = topFun = eval(f, globalenv())
  }

  if(is.function(f)) {
      params = formals(f)
                                              # .ignoreDefaultArgs but then identifies params as non-local variables.
      defaultValuesProcessed = structure(rep( FALSE, length(params)), names = names(params))
  } else {
      params = list()
      defaultValuesProcessed = logical()
  }
  
  if(typeof(f) == "closure") {

      if(!.ignoreDefaultArgs) {
          # process the parameters. Why did I only work on those that have no default value???
          #  Seems to give the wrong answer.
          # Since this is inside .ignoreDefaultArgs, were we processing those parameters with
          # default values separately?
          if(old) #XXXX  remove when we are certain the new/non-old way is okay.         
              localVars = names(params)[sapply(params, function(x) is.name(x) && x == "")]
          else
              localVars = names(params)
          
          f = body(f)
     }
  } 


    fun(f, fun)

    if(!.ignoreDefaultArgs && any(!defaultValuesProcessed))
        # this doesn't update defaultValuesProcessed. May want to set these elements to TRUE.
      lapply(params[!defaultValuesProcessed], fun, fun)
  
    varsByFun = varsByFun[ setdiff(names(varsByFun), c("for", "if", "{"))]

    #XXX This doesn't catch the case that a function is defined and called before the variable
    # it references in the parent function is defined.

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


isColonCall =
function(e)
    is.call(e) && isSymbol(e[[1]], c("::", ":::"))
# was is.call(e) && is.name(e[[1]]) && ((tmp <- as.character(e[[1]])) == "::" || tmp ==  ":::")


getGlobalFunctions =
function(x, ...)   
    UseMethod("getGlobalFunctions")

getGlobalFunctions.GlobalUses =
    #
    # method for object created/returned by getGlobals
    #
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

rewritePipeCall =
    # rewritePipeCall(quote( x %>% foo))
    # rewritePipeCall(quote( x %>% foo(2)))
    # rewritePipeCall(quote( x %>% foo(2, .)))
function(e)
{
    # rewrite the pipe call from lhs %>% rhs(x, y) to rhs(lhs, x, y)

    e0 = e
    e = e[[3]]

    # if it is data %>% funName, then have to change that to funName(data)    
    if(is.symbol(e)) 
        e = substitute(foo(), list(foo = e))

    # Handle a .

    tmp = as.list(e0[[3]]) [-1]
    
    w = sapply(e, isSymbol, ".")
    if(any(w)) {
        # e0[[2]]
        e[w] = replicate(sum(w), e0[[2]], simplify = FALSE)
    } else  {
        e[[2]] = e0[[2]]
        # other args
        if(length(tmp) > 0)
            e[ seq(along.with = tmp) + 2L ] = tmp    
    }
    
    e
}



getFunDef =
function(funName, funDefs)
{
    if(funName %in% names(funDefs))
        return(funDefs[[ funName ]])
    
    tryCatch(get(funName),
             error = function(e) NULL)    
}
