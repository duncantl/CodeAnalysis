# Rewriting Code with codetools::walkCode()

The idea is to reassemble the language objects and collect 
information.

Examples of what we want to do are 

+ √ change a variable/parameter name and all of its uses.
+ √ add an argument to specific calls
+ √ remove calls, e.g.,
    + nested functions of the form `fn = function()...`
	+ rewrite `if(TRUE) ... else ...` to remove the `else ...`
    + assignments to unused variables
	
These operations will allow us to implement

+ constant propagation/replacement
+ remove named nested functions and make them separate functions that can be reused and tested separately.
   + identify which non-local variables they were accessing
   + add corresponding parameters to these new functions
   + arrange for calls to those functions in the original function 
       to pass these non-local variables.


## Moving Functions out of a Function 

The following is a very simple example, intentionally so.

```
foo =
function(x, lambda = 2)
{
   alpha = lambda/nrow(x)
   
   do = function(val)
             val * alpha
			 
   apply(x, 2, do)
}
```
We want to extract `do` as a separate function.

It needs alpha, so we would add that as an additional parameter.
We write it as
```
do = function(val, alpha)
         val * alpha
```

Now, we need to add `alpha` in the calls to `do`.
We'd rewrite `foo` as
```
foo =
function(x, lambda = 2)
{
   alpha = lambda/nrow(x)
   apply(x, 2, do, alpha)
}
```


A simpler version is when the original function calls the nested function directly.
```
foo =
function(x, lambda = 2)
{
   alpha = lambda/nrow(x)
   
   do = function(val)
             val * alpha
			 
   a = do(x[, 1])
   b = sum(do(x[ !is.na(x[ ,2]), 2]))   
}
```
We would rewrite this as
```
foo =
function(x, lambda = 2)
{
   alpha = lambda/nrow(x)
  
   a = do(x[, 1], alpha)
   b = sum(do(x[ !is.na(x[ ,2]), 2], alpha))   
}
```



# Todo

### addParams

+ [test] implement the changeParamName
   + character vector of c(name = newName,...) and rewrite() like in propagate.R

+ passGlobals
   + rewrite calls to any of the new functions, adding the named arguments.
