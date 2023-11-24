In the indexWalkCode, I defined functions getParent, getAncestors, getSiblings and getByIndex
to each take the AST and the index path in that order.
These are consistent with each other which is good.

When using these (in FunctionsInFunctions.md), I need to use these when looping over a collection of
index paths, e.g.,
```
lapply(idx, function(i) getParent(ast, i))
lapply(idx, function(i) getAncestors(ast, i))
```

It is more convenient to reverse the order of the arguments so we can use
```
lapply(idx, getParent, ast)
lapply(idx, getAncestors, ast)
```

This is a  small benefit (and we can use the notation `\(i)` to avoid `function(i)`)
but it is convenient.
It is also probably appropriate to think of the IndexPath object as being the primary object
and the AST as the context. Indeed, if we attached the AST as an attribute to each IndexPath,
we wouldn't need to provide that context.

So we'll change the signatures.
Before we do this, we want to know how much code we will have to change.
If it is too much, we may decide not to make the change.

Of course, if these functions are being in other code outside of the package, they will have to
change too.
But for now, let's focus on the code in this package.
We need to find all calls to these four functions.
There are 3 places we need to look for such calls

+ the files in the R/ directory
+ the code in the tests/ directory
+ code in the examples in the Rd files


We'll start by creating a vector of the function names of interest to avoid repeating them in
several places:
```
idxFuns = c("getParent", "getAncestors", "getSiblings", "getByIndex")
```

Note: Rather than finding calls to any of these functions, we probably
want to find any reference to these symbols. 
`findCallsTo()` finds direct calls to a function name.
However, it doesn't include indirect calls via do.call or any of the apply functions.


Note:  What about insertByIndex()? Should we change the order of its arguments to be consistent?
It is less important as the first argument is the object to be inserted. So we won't loop over.
However, we might want to use `mapply( , newVals, indices, MoreArgs = list(ast = ast))` and would
again have to write an intermediate function to map the arguments to the first and third.


## R/ directory

```{r}
fns = getFunctionDefs("R")
k = lapply(fns, findCallsTo, idxFuns)
sapply(k, length)
```

## tests/ directory

```{r}
rf = list.files("tests", pattern = "\\.R$", full = TRUE)
e = lapply(rf, parse)
k2 = lapply(e, findCallsTo, idxFuns)
sapply(k2, length)
```


## Examples from Rd files

Looking at the code in the `example()` function,  we find
the call to `tools::Rd2ex()`.
This takes the name of the Rd file and a place to write the output.
It would be simpler/better if it returned the text of the example rather than indirectly putting it
in a file. However, we can use this but put it into a textConnection.

```
rd = list.files("man", pattern = "\\.Rd$", full = TRUE)
egs = lapply(rd, function(file) { 
             con = textConnection("eg", "w", local = TRUE)
             on.exit(close(con))
			 tools::Rd2ex(file, con)
			 parse(text = textConnectionValue(con))
		 })
names(egs) = rd		 
```

```
kegs = lapply(egs, findCallsTo, idxFuns)
sapply(kegs, length)
```

Of course, we will also have to change the usage sections 
and should reverse the order of the items in the arguments sections.



## Total number of calls to change?

How many calls do we have ?
```
length(unlist(list(k, k2, kegs)))
```
At the time of writing, we have 16 calls in total.

And we have to change the definitions of the functions themselves.


## R/ directory

```{r}
k = k[sapply(k, length) > 0]
names(k)
```
This identifies 2 calls to `getParent()` in `getByIndex()` and `getSiblings()`.
(We could use `pkgCodeInfo()` to get a data.frame describing all the functions in the R/ directory
and know which files these are in. However, we already know this and will make the changes manually.)


## tests/ directory

For the tests/ directory, we have
```{r}
k2 = k2[sapply(k2, length) > 0]
names(k2)
```
which identifies 3 files
```
[1] "tests/predParent.R"  "tests/rewrite2.R"    "tests/rewriteCode.R"
```
and 5 calls with 3 in predParent.R and 1 in each of the other 2.

Now we'll find which functions are called in which files:
```
sapply(k2, function(x) sapply(x, function(x) as.character(x[[1]])))
```
```
$`tests/predParent.R`
[1] "getParent"  "getByIndex" "getParent" 

$`tests/rewrite2.R`
[1] "getByIndex"

$`tests/rewriteCode.R`
[1] "getByIndex"
```


## man/ and Rd files

```{r}
k3 = kegs[sapply(kegs, length) > 0]
names(k3)
```
```
[1] "man/getByIndex.Rd"    "man/getParent.Rd"     "man/indexWalkCode.Rd" "man/insertByIndex.Rd"
```
The number of calls in each file is
```{r}
sapply(k3, length)
```
```
   man/getByIndex.Rd     man/getParent.Rd man/indexWalkCode.Rd man/insertByIndex.Rd 
                   2                    4                    2                    1 
```

And the functions being called in each are 
```
sapply(k3, function(x) sapply(x, function(x) as.character(x[[1]])))
```
```
$`man/getByIndex.Rd`
[1] "getParent"  "getByIndex"

$`man/getParent.Rd`
[1] "getParent"    "getByIndex"   "getAncestors" "getAncestors"

$`man/indexWalkCode.Rd`
[1] "getByIndex" "getParent" 

$`man/insertByIndex.Rd`
[1] "getByIndex"
```

So now we know what we have to change.
We can also rerun this code at the end to find the calls and then programmatically ensure that the changes
are correct.
