
Some people like to write helper functions within the primary function that uses them.  For example,
the `summarize_CRAN_check_status()` in the tools package defines the summarize_results(),
summarize_details() summarize_issues() and summarize() functions near the beginning of that
function.  
This avoids cluttering the workspace with functions that are only used within function. 
These functions can also access non-local variables that are in the primary function. This 
simplifies calling these helper functions as we don't have to pass these additional arguments.
However, within a package, we hide these by simply not exporting these internal helper functions.  This style
means we cannot easily test these functions separately from the primary function.  Similarly, nobody
else can reuse these functions (including the author) unless they manually extract them from the
text of the code and create a copy that may diverge from the original.  Generally, often we only
want to define a function within the body of a function when we are creating a closure that can
update non-local variables.

An example of defining functions  within functions is in
the file FAO56_dualcropcoeff.R.
This starts with a collection of comments and then has some code that creates
data objects within the global works space.
Next we define a function named FAO56DualCropCalc.
In this function, we define 36 functions, e.g. CropParametersDefine, KcbDefine,
alfalfa.Kcb.cycles, KepCalc, EpCalc, etc.

In an effort to make this code more efficient, we wanted to experiment
with different implementations of some of these functions. Also, we wanted to 
get a better understanding of the code and see which pieces relied on other pieces
and see the actual computations within the primary function
FAO56DualCropCalc().
So we want to extract all the function definitions in the body of the primary function,
specifically those that don't use the <<- operator to modify non-local variables.
In our example, none of the functions do this, so we want to extract all 36 functions.
We'll move these functions to a separate file and also remove them from
FAO56DualCropCalc().

To extract these functions, we don't want to run the code in
FAO56_dualcropcoeff.R as the code to create the data objects takes some time.
We could manually edit the file and move the function 
FAO56DualCropCalc to a separate file.
However, we will find the definition for FAO56DualCropCalc in 
FAO56_dualcropcoeff.R and process the code directly.
To do this, we use 
```
kode = parse("../extractFuns/example/FAO56_dualcropcoeff.R")
isFunction = function(x) is(x, "<-") && is.call(x[[3]]) && x[[3]][[1]] == "function"
isFun = sapply(kode, isFunction)
```
This finds the one expression in the file that defines a function and assigns it to 
a variable.
So we take that expression and evaluate the right-hand side which is the
function definition:
```
f = kode[[which(isFun)]]
fun = eval(f[[3]])
```

We now have the function which contains the calls to define the 36 internal functions.
We pass this to the extractFunctions() function:
```
library(rstatic)#XXX FIX
e = extractFunctions(fun)
```
extractFunctions() returns a list with two elements.
The first (`fun`) is the update primary function with the function definitions removed.
The second element (`externalFunctions`) is a list of those function definitions,
specifically the expressions of the form `name = function(...) ...`, i.e. the assignments.
We still have to evaluate these expressions to both define the function and assign it to the
variable name.

We can write the externalFunctions to a separate file or keep them in the R session.

Of course, some of these functions may use variables defined within their
enclosing/primary function. When these functions are defined separately, we
would have to arrange to call these functions with these extra arguments.  Also,
we have to add extra parameters to these functions to accept these additional
arguments.  This is topic of section ??.




