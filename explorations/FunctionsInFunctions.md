<!-- See ~/Book/tools_package/tools.xml for something related -->

#
Consider the tools:::.check_packages function.
It is the "longest" in the R core and recommended packages, in terms of the number of calls.
It also contains many functions defined within the body of this function.
```{r}
inf = getFunctionDefs(tools:::.check_packages)
```
We want to find out 
+ which are anonymous? 
+ which are named?
+ how deep is the nesting?
+ which use non-local variables?
   + defined in the parent function
   + globals and if so where.
+ which update non-local variables?
+ which can we move out of the parent/host function
   + how do we update calls to these to provide the non-local variables.


```
fn = tools:::.check_packages
library(indexWalkCode)
pred = function(x, idx, ast, ...) 
          isCallTo(x, "function")  # is.call(x) && isSymbol(x[[1]], "function")
# or simpler
pred = mkIsCallTo("function")		 
idx = indexWalkCode(fn, pred)
```

We could have collected the code objects also, but we can get them with
```
fns = lapply(idx, function(i) getByIndex(fn, i))
```

We want to look at the parent expression to see if they are an assignment.
```
p = lapply(idx, function(i) getParent(fn, i))
isAssigned = sapply(p, isCallTo, "<-")
table(isAssigned)
```

So there are 27 that aren't assigned immediately. 
So they are used in a call. However, the following
```
foo(function(x) x+1)
tryCatch(file_path_as_absolute(substr(install, 7L, 1000L)), error = function(e) "")
var <- if(cond) function(x) x+1 else function(x) x - 1
```
are very different. The first two involve an anonymous function while the third
is merely a conditional assignment to `var`.


```
p2 = lapply(idx[!isAssigned], function(i) getAncestors(fn, i)[[2]])

ancestors = lapply(idx[!isAssigned], function(i) getAncestors(fn, i))
isAssigned2 = sapply(ancestors, function(x) any(sapply(x, isCallTo, c("=", "<-"))))
table(isAssigned2)
wass = sapply(ancestors, function(x) min(which(sapply(x, isCallTo, c("=", "<-")))))

ty = mapply(function(x, i) sapply(x[1:i], class), ancestors, wass)
var = mapply(function(x, i) x[[i]][[2]], ancestors, wass)
```

Maybe look down the tree and find where we have an assignment
that is conditional


## Check for explicit calls to assign()

```{r}
stopifnot(length(findCallsTo(fn, "assign")) == 0)
```

## Check for calls to eval()

```{r}
stopifnot(length(findCallsTo(fn, "eval")) == 0)
```


## Convert the calls to function to functions

```{r}
funs = lapply(fns, eval)
```

## Non-local Assignments

Let's start with the functions that have non-local assignments.
We won't be able to move these outside of the host function.
They need to see the variables in the environment when they are defined
and that is the call frame specific to the invocation of the host function.

```{r}
nla = lapply(funs, findCallsTo, "<<-", parse = FALSE)
w2 = sapply(nla, length) > 0
table(w2)
```
So there are 10 functions within .check_packages (at some level of nesting) that
may perform non-local assignment.


What variables/expressions are these functions assigning to?
```
k = nla[w2]
#lapply(k, function(x) sapply(x, function(x) x[[2]]))
table(unlist(unname(lapply(k, function(x) sapply(x, function(x) deparse(x[[2]]))))))
```
```
         .messages               .msg                bad do_build_vignettes        do_examples          Log$stars 
                 4                  2                  2                  1                  1                  4 
       no_examples 
                 1 
```
Log$stars is not a variable but an element in a list.
The others are variables.


Not that it matters, but we can see what are they assigning:
```
unname(lapply(k, function(x) structure(sapply(x, function(x) deparse(x[[3]])), 
                                        names = sapply(x, function(x) deparse(x[[2]])))))
```



## Non-local Variables

```
funs2 = funs[!w2]
nlv = lapply(funs2, function(f) codetools::findGlobals(f, FALSE)$variables )
nnlv = sapply(nlv, length)
w3 = nnlv > 0
table(w3)
```
48 of the 68 functions refer to non-local variables.

Some of these could be false positives from findGlobals.
We can use getGlobals to see:
```
nlv2 = lapply(funs2, function(f) getGlobals(f, FALSE)$variables )
nnlv2 = sapply(nlv2, length)
w4 = nnlv2 > 0
table(w4)
```
We get the same number of functions, but not the same number of non-local variables in each
function.
Howver, getGlobals gives all instances of a non-local variable, not the unique set.
When we look at the 
```{r}
a = mapply(function(x, y) setdiff(x, y), nlv, nlv2)
b = mapply(function(x, y) setdiff(y, x), nlv, nlv2)
table(sapply(a, length), sapply(b, length))
```
```
     0  1
  0 63  1
  1  3  0
  3  1  0
```
So there are 5 functions in this set where findGlobals and getGlobals disagree.
<!-- We fixed getGlobals to handle aar$comment = value  -->

```
ww = mapply(function(x, y) length(c(setdiff(x, y), setdiff(y, x))), nlv, nlv2) > 0
```
```
a[ww]
```

sort(unique(codetools::findGlobals(funs2[ww][[1]], FALSE)$variables))
sort(unique(getGlobals(funs2[ww][[1]])$variables))

They agree on 4, except findGlobals finds unname and getGlobals comment.
+ unname - The call to unname is in an lapply(, unname). So getGlobals is correct in not identifying that.
<!-- getGlobals() was including  comment from `aar$comment <- lapply(aar$comment, unname)` but fixed now. -->



For all but one of these 5 functions, the difference between findGlobals and getGlobals is due to 
symbols being used in lapply/sapply/vapply calls.
One (the 5th), however, illustrates that findGlobals() doesn't detect a variable (`install`)
that is assigned locally, but used well before that in the code. This is an error.
getGlobals() identifies this non-local variables.


