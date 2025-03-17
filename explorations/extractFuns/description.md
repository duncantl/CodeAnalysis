
Some people like to write helper functions within the primary function that uses them.  For example,
the `summarize_CRAN_check_status()` in the tools package defines the `summarize_results()`,
`summarize_details()`, `summarize_issues()` and `summarize()` functions near the beginning of that
function.
This avoids cluttering the workspace with functions that are only used within function. 
These functions can also access non-local variables that are in the primary function. This 
simplifies calling these helper functions as we don't have to pass these additional arguments.
However, within a package, we hide these by simply not exporting these internal helper functions.  This style
means we cannot easily test these functions separately from the primary function.  Similarly, nobody
else can reuse these functions (including the author) unless they manually extract them from the
text of the code and create a copy that may diverge from the original.  Generally, we often only
want to define a function within the body of a function when we are creating a closure that can
update non-local variables.

An example of defining functions  within functions is in the file FAO56_dualcropcoeff.R.
(This comes from github.com/smdevine/GreenWater.)
This starts with a collection of comments and then has some code that creates
data objects within the global works space.
Next "we" define a function named FAO56DualCropCalc.
In this function, we define 36 functions, e.g. CropParametersDefine, KcbDefine,
alfalfa.Kcb.cycles, KepCalc, EpCalc, etc.

In an effort to make this code more efficient, we wanted to experiment
with different implementations of some of these functions. Also, we wanted to 
get a better understanding of the code and see which pieces relied on other pieces
and see the actual computations within the primary function FAO56DualCropCalc().
So we want to extract all the function definitions in the body of the primary function,
specifically those that don't use the <<- operator to modify non-local variables.
In our example, none of the functions do this, so we want to extract all 36 functions.
We'll move these functions to a separate file and also remove them from
`FAO56DualCropCalc()`.

To extract these functions, we don't want to run the code in
`FAO56_dualcropcoeff.R` as the code to create the data objects takes some time.
We could manually edit the file and move the function 
`FAO56DualCropCalc()` to a separate file.
However, we will find the definition for `FAO56DualCropCalc()` in 
FAO56_dualcropcoeff.R and process the code directly.
To do this, we use 
```
fun = getFunctionDefs("../extractFuns/example/FAO56_dualcropcoeff.R")[[1]]
class(fun)
```

We now have the function which contains the calls to define the 36 internal functions.
We pass this to the extractFunctions() function:
```
library(CodeAnalysis)
e = extractFunctions(fun)
```
<!--  Doesn't work with the linear-ir branch of rstatic Jun 7, 2018. -->
extractFunctions() returns a list with two elements.
The first (`fun`) is the updated primary function with the function definitions removed.
The second element (`externalFunctions`) is a list of those function definitions,
specifically the expressions of the form `name = function(...) ...`, i.e. the assignments.
We still have to evaluate these expressions to both define the function and assign it to the
variable name.

We can write the externalFunctions to a separate file or keep them in the R session.

Of course, some of these functions may use variables defined within their
enclosing/primary function. When these functions are defined separately, we
would have to arrange to call these functions with these extra arguments.  Also,
we have to add extra parameters to these functions to accept these additional
arguments.  This is topic of section ?? (and now below).

(See eg2.R and eg.R.  Also, see ../globalsRewrite/)


## Identifying the Non-Local Variables in the Extracted Functions

We first evaluate the code for each of the extracted functions to create
them as regular functions (not parse trees):
```r
env = new.env()
invisible(sapply(e$externalFunctions, function(f) eval(f, env)))
names(env)
```

Next, we find the global variables in each of these extracted functions.
```r
globals = lapply(ls(env), function(var) codetools::findGlobals(get(var, env), FALSE))
names(globals) = ls(env)
```

There are only 12 of the 36 functions that reference external variables:
```r
ng = sapply(globals, function(x) length(x$variables))
w = ng > 0
table(w)
```



The function `GreenWaterIrr1Calc` refers to the now non-local variable Jdev.
We need to add that as a parameter to the function.
The function `mkGlobalsLocal` does this.
```
f = mkGlobalsLocal(env$GreenWaterIrr1Calc)
```
Here we see Jdev has a new parameter named .Jdev.
All of the code in this function that originally referred to Jdev now refers to .Jdev.
We can either make the Jdev the default value for .Jdev or alternatively provide no
default. In this case, it makes sense to provide no default.

We can pass all of the extracted functions to mkGlobalsLocal in one step and will only update
the relevant ones, i.e. that refer to global variables:
```
updatedFuns = mkGlobalsLocal(.funs = as(env, "list", strict = FALSE))
```
<!-- Currently only returns the 12, not all.
     [fixed] Need method for ast_traverse() for Parenthesis. Inherits from Application?  Poor choice 
     and the Return node now seems to have a read and write. This seems to be very specific to SSA.
  -->



## Updating the Original/Primary Function to Pass Extra Arguments

We also need to change the calls to `GreenWaterIrr1Calc` in the (now modified) original function.
We first turn this code into an actual function by evaluating it:
```
prim = eval(e[[1]], env)
```





