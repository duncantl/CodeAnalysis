

## findCallsTo() 

+ If not given a value for `funNames`, will find all calls.
+ `funNames` can be a character vector and will only return calls to those functions.
+ can be limited to look in just the current function and not recurse into
  sub-function definitions. Use maxFunDepth.


## getFunctionDefs() 

+ can take a 
    + directory, 
	+ vector of file names
	+ a parsed script
	+ a function object
	
	

## callGraph()

+ Determines which function calls which other functions
   + Does not determine how many times one function calls another, just a simple binary 0 or 1.

+  Can get the number of calls with 
```
defs = getFunctionDefs("R")
cfuns = lapply(defs, \(x) getGlobals(x)$functions)
cfuns = unlist(cfuns)
cfuns = cfuns[ cfuns %in% names(defs)]
dsort(table(cfuns))
```


## mkModifyCodeWalker()

+ Allows caller to specify a function to modify or remove a language element in an AST.


## S3Assignments

+ Finds all the S3 class names used/referenced in code.

## getAttributeNames()

+ gets the names of all attributes used in `attr()` or `structure()` calls.
+ currently takes a language object, not a file or directory.







###

+ findAssignsTo is a call to findCallsTo with a specific predicate function passed to mkCallWalkerPred().
+ findCallsTo just arranges to get the code to walk and calls walkCode() with the walker it is
  given.
    + if not given an explicit walker, it creates one with mkCallWalkerPred() or mkCallWalker()
