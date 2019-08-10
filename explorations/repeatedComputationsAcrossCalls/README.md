Consider Scott Devine's  [GreenWater repository](https://github.com/dsidavis/GreenWater) repository
and specifically the Analysis/chp1.Muaggregate.by.year.R file. (Copied to here for convenience.)

Firstly, there appears to be a missing } at line 381.


Manually reading the code, we see that there is 
+ a call to setwd() with a global variable that doesn't change and then
+ a sequence of 6 calls to read.csv() (interspersed with a few other commands)
+ another call to setwd() - `setwd(file.path(original.resultsDir, 'climate_summaries'))`
  + is this constant across calls
+ another call to read.csv


Other calls to read.csv() are within a loop and the name of the directory depends on cropname which is a paramter and so
changes across calls.


We want to programmatically identify these calls to read.csv() where the file is a literal.
These are invariant across the 9 calls to data.to.allyrs().
Accordingly, we should read them once and pass them in each call to data.to.allyrs().


# Code Analysis

We don't want to source() or evaluate this code. We want to statically analyze it.
```
library(CodeDepends)
library(CodeAnalysis)
```

```
sc = readScript("chp1.Muaggregate.by.year.R")
info = as(sc, "ScriptInfo")
```

Find the top-level function definitions
```
w = sapply(sc, function(x) inherits(x, c("=", "<-")) && is.call(x[[3]]) && x[[3]][[1]] == "function")
```

```
funs = sapply(sc[w], function(e) eval(e[[3]]))
names(funs) = sapply(sc[w], function(e) as.character(e[[2]]))
```


Let's look at data.to.allyrs. We'll first find which global variables
it uses.
```
unique(getGlobals(funs$data.to.allyrs)$variables)
```
```
[1] "modelscaffoldDir"    "F"                   "original.resultsDir"
[4] "clean.resultsDir"    "rbind"               "sum"                
[7] "points.resultsDir"  
```

The F should be FALSE. sum and rbind are most likely calls in do.call or lapply.
   <!-- Note: add knowledge of do.call to getGlobals -->
```
do.call(rbind, lapply(prism.by.year, sapply, sum))
```
This is quite a sophisticated call.


The *Dir variables are the ones that interest us.
We find these and where they are used with
```
ff = to_ast(funs$data.to.allyrs)
idx = find_nodes(ff, function(x) is(x, "Symbol") && grepl("Dir$", x$ssa_name))
sapply(ff[idx], function(x) x$parent$parent$fn$name)
```
```
 [1] "setwd"     "file.path" "file.path" "file.path" "file.path"
 [6] "file.path" "file.path" "file.path" "file.path" "file.path"
[11] "file.path"
```


The call to setwd() is `setwd(modelscaffoldDir)`.
Is modelscaffoldDir a constant for this script? across calls to data.to.allyrs()
```
w = sapply(info, function(x) "modelscaffoldDir" %in% x@outputs)
table(w)
```
So there is only one, initial assignment to modelscaffoldDir in the script and
so is constant (unless there is some assignment to it in a function).


Returning to our function data.to.allyrs(), we can find the calls to read.csv():
```
idx = find_nodes(ff, function(x) is(x, "Call") && x$fn$ssa_name == "read.csv")
```

Find the calls to read.csv() where the file is a literal and not a variable.
```
idx = find_nodes(ff, function(x) is(x, "Call") && x$fn$ssa_name == "read.csv" && is(x$args[[1]], "Character"))
```

The file names are
```
sapply(ff[idx], function(x) x$args[[1]]$value)
```


There are many calls to setwd()
```
ff[find_nodes(ff, function(x) is(x, "Call") && x$fn$ssa_name == "setwd")]
```
So we have to trace the current directory to find out where these files are being read from.




## Can we evaluate the calls to data.to.allyrs() in parallel.

We have to determine whether there are any race-conditions in the function.
+ Could they be writing to the same output file?
+ Could one write a file that a later call reads?

we can find all the calls to write.csv() in the function
```
idx = find_nodes(ff, function(x) is(x, "Call") && x$fn$ssa_name == "write.csv")
```



