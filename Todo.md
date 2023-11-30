<!-- 
  Count the number left to do.
  ll =readLines("Todo"); length(grep("^\\+", split(ll, cumsum(grepl("^#", ll)))[[1]]))
-->
+ getFunctionDefs for call
  + example for ifCall and whileCall gives very nested list.
  + examples for some types do recursive regardless
     + `getFunctionDefs(quote(function(x)  function(mu, sd) prod(dnorm(x, mu, sd))))`
        returns both when recursive is either TRUE or FALSE

+ callGraph(".") for CodeAnalysis/R gives an error `cannot change value of locked binding for
  'isAssignReturn`
    + trying to source() the code into an environment but the setGeneric is not given the target environment.
    + fix the environment for setGeneric/setMethod or 	    
	+ √ provide setGeneric/setMethod in the environment so these are called instead of the ones in
      methods.
	   + √ put the functions into the target namespace or a list.

+ If add a parameter to a function, what calls do we need to change.
  + See R/addParam.R  
  + did this recently, so finalize and make easy to use.
  + example, isLHS and envir in function isIndirectCall

+ getGlobals() doesn't detect local variable in if() could be a global
  + which example???
  + same with  x < 0 || (w <- any(is.na(x))) when x is < 0 and second term not evaluated.

+ Find unused parameters and local variables 
   + Check if the new findAssignsTo() helps this, i.e., the complex assignments.
   + see findUnusedParams
   + can we adapt getGlobals() to identify local variable creation and then if it is used.
   	  + add a field to getGlobals() output which indicates if local variable was used - either as
             return value or in RHS of another expression.

+ isIndidrectCall - errors in call to match.call()
   + remove the try() and run procPackages(returnsFunction)
   + methods package has body(, envir = ..)
      + actually this is a body<- call.
	  + also use the package's environment
   + see findCallsTo issue about environment for get()
   + √ for now, tryCatch() and if isLHS is NA or TRUE, try the `funName<-` version of the function.
       + XXX ideally pass isLHS down through the call stack.
	   + we can't tell from the call itself if it is the LHS
     	   + need argument or 
		   + √ could put an attribute on this from higher up the call stack.
   
+ [check] programmatically determine if a function returns a function - returnsFunction()
  + grDevices:::.select_device
     + doesn't include pdf as one of the possible functions being returned.
	 + not following the definition of local variables within the function when 
	   the return value is a symbol.
  + fix to deal with processing assignments in subfunctions and not the function itself.
     + need to detect updates/replacement function assignments, e.g. attr(f, "foo") = x and f$foo =
       x and f[["foo"]] = x and chains.

+ [check] findCallsTo - extend to identify do.call and *apply calls. 
   + fix isIndirectCall to determine how we resolve a function name. 
      + needs environment for call to get() or pass the functions directly.
   + √ consolidate with isCallTo()
       + [need to do rstatic] that mixes rstatic and language objects. Add to language part but should update
	      rstatic part also.
   + √ [check] Handle pkg::foo and pkg:::foo cases for indirect, e.g. do.call(base::rbind)
   + √ when processing a call, check if isCallTo(x, OneOfTheseFunctions)
       and if so, is the FUN argument (or corresponding argument) a call to the
  	   target functions.

+ getFunctionDefs() - have it find functions in setMethod()
   + √ already works with recursive = TRUE.
   + perhaps put better name on "these" (what ?) methods and generics.
     + need state, or ability to go back up tree.

+ circular references in parameters 
   + find parameters with default values that refer to local variables
      and check if the parameter is used before those local variables are created.
   + See explorations/findSelfRef.R.



+ fix callGraph(fun) and the name used is obj. (meaning what??).
   + obj is the name of the first argument.

+ implement findOS()/mkOSWalker for finding code that depends on the platform/OS

+ If a function has a literal externalptr inlined, then getGlobals() fails in lapply(els, fun, w)

+ Fix mkGlobalsLocal() to optionally not add a default value to a parameter
   e.g. not replace Jvar with .Jvar = Jvar

+ Have mkGlobalsLocal() return the entire list, not just the ones that were changed.
   Make this an option.

+ mkGlobalsLocal() not changing references inside return() clause. See rstatic probably.

+ for loop concatenation, rewrite the code.
   We need type information about the elements to be able to initialize the answer vector.

+ [low] code analysis example (unrelated to highlighting, just the example)
  + highlight package getStyleFile() has
  ```
  if (grepl(sprintf("%s$", extension, ignore.case = TRUE),  name))
  ```
  + the ignore.case is in the call to sprintf() and the command
    gives a warning about ignoring an argument to sprintf.
  + The ignore.case should be outside the sprintf() and in the call to grepl()
     + but since sprintf() has ... it is ambiguous, but not really
  + check whether a named argument in a call makes sense in the parent call.


+ [check/robustify] isAssignTo - make it handle complex left hand side.
   + √ Look at the function isComplexAssignTo. Make more robust.
   + See foo in tests/findAssignsTo.R.
   + Uses isAssignTo
     + So currently just checks if the LHS is the symbol. 
	 + add a version of this that handles complex assignments.
   + problem is `x$y <- value ` is a call to `$<-`(x, y) and misses value.  Need to go back up the
     tree.	 
	 + But actually `x$y <-value` is parsed as `<-(x$y, value)`
   + findAssignsTo(findCallsTo, "isEnv") doesn't find the assignment in the if condition.
      + old todo item and findCallsTo code no longer has isEnv
      + but issue is *probably* if( isEnv <- is.environment(x))
      + findAssignsTo2 seems to work.



# Done

+ findFunctionDefs()/getFunctionDefs() doesn't handle  name = name2 = function() ...
   + √ getFunctionDefs() gets it right for tests/chainedAssigns.
   + findFunctionDefs() returns list.
   + Old
     + only captures name and not name2
     + e.g. RDCOMEvents/eventServer.S
     + shouldn't need recursive = TRUE to find name in name = name2 = function() ..
     + works now, but not with recursive = TRUE

+ Re. above - should we get rid of findFunctionDefs()
    + √ fixed -  getFunctionDefs() doesn't know about Vectorize/vectorize
	+ √ getFunctionDefs() knows about (top-level) if(FALSE)
	   + see `getFunctionDefs("tests/chainedAssigns.R")`

+ Fix message from getGlobals
```
In procIndirectFunCall(e, funName) :
       cannot currently determine function in empty call to match.call. That uses the context of the call.1
```
   + see base::autoloader
      + findSelfRef.R and running on all packages to reproduce issue.
   + remove this message. It occurs with calls to match.call() and formals() with no
     arguments. There are no variables there - global or local.

+ √ false positive for global variables in R CMD check for textConnection("bob", "w", local = TRUE); bob
  + This is an R issue.
  + √ We can make getGlobals() understand it.

+ removeAfterReturn  should work recursively to process blocks within functions.

+ remove unused parameters - see findUnusedArgs/findUnusedParams. also unusedParams

+ remove unused assignments if we can tell there are no side effects of the RHS.



## getFunctionDefs v findFunctionDefs

+ Vectorize
   + Yes getFunctionDefs() - after changes
   + Yes findFunctionDefs()
+ chained assigns x = y = function
   + Yes getFunctionDefs()
+ if(FALSE)
   + Yes getFunctionDefs()
+ setGeneric
   + No getFunctionDefs()
+ setMethod()
   + No getFunctionDefs()
+ handle different types of inputs
   + √ - overall:   √ filename, √ directory name, √ environment, √ list, √ function, call, ....??
   + Yes getFunctionDefs()
+ recursively process functions to find subfunctions
   + Yes if given a function, file, expression.
+ recursively follow source() commands
   + No.


findFunctionDefs picks up 
  x$fun = function 
which is not necessarily good  


+ In tests/ directory
```
z = getFunctionDefs("getFunctionDefsEg2.R") # file
z3 = getFunctionDefs(".")   # directory
e = new.env(); source("getFunctionDefsEg2.R", e); ze = getFunctionDefs(e)  # environment
z4 = getFunctionDefs(as.list.environment(e, TRUE)) # list
getFunctionDefs(z4$rec) # function object
getFunctionDefs(quote(function(x)  x + 1)) # call
getFunctionDefs(quote(x <- function(x)  x + 1))  # assignment call

z = getFunctionDefs("getFunctionDefsEg2.R", recursive = TRUE)
e = parse("getFunctionDefsEg2.R")
z2 = getFunctionDefs(e)
z2r = getFunctionDefs(e, recursive = TRUE)


```
