chp1.Muaggregate.by.year.R taken from Scott Devine's  [GreenWater repository](https://github.com/dsidavis/GreenWater) repository
and specifically the Analysis/chp1.Muaggregate.by.year.R file. (Copied to here for convenience.)


The purpose of this is to take a script and find the expressions that read data.
We can then examine those to see how we might modify/rewrite them to read the data in chunks
that can be run in parallel.

This also relates to finding input and output files in [file dependencies](../file_dep)
explorations.

This uses static analysis.


We start with a script, rather than a function
We want to find 
+ expressions that call known input/data-reading functions, e.g., read.csv, scan, ...
+ find aliases for these functions that are used, e.g.,
  ```
   f = read.csv
   f(file)
  ```
  but not aliases that are not used
+ function definitions in the script that themselves call known input/data-reading functions
  that are then used
  

## Status
This feels akwardly written and includes symbols in the results that we may not want.


```
library(rstatic)  
source("findReadDataCalls.R")
```


## readEg.R
```
kk = to_ast(parse("readEg.R"))
e = findReadDataCalls( kk)
```

```
[[1]]
<Call> $args $fn $parent
read.csv(file) 


[[2]]
<Assign> $parent $read $write
f = read.csv 


[[3]]
<Call> $args $fn $parent
f("jane.csv") 


[[4]]
<Call> $args $fn $parent
getData("data.txt") 


[[5]]
<Call> $args $fn $parent
lapply(files, getData) 
```


## chp1.Muaggregate.by.year.R
```
kk2 = to_ast(parse("chp1.Muaggregate.by.year.R"))
findReadDataCalls( kk2)
```
```
[[1]]
<Call> $args $fn $parent
data.to.allyrs("pistachios", "Pistachios") 


[[2]]
<Call> $args $fn $parent
data.to.allyrs("almond.mature", "Almonds") 


[[3]]
<Call> $args $fn $parent
data.to.allyrs("walnut.mature", "Walnuts") 


[[4]]
<Call> $args $fn $parent
data.to.allyrs("grapes.table", "Grapes") 


[[5]]
<Call> $args $fn $parent
data.to.allyrs("grapes.wine", "Grapes") 


[[6]]
<Call> $args $fn $parent
data.to.allyrs("alfalfa.CV", "Alfalfa") 


[[7]]
<Call> $args $fn $parent
data.to.allyrs("alfalfa.intermountain", "Alfalfa") 


[[8]]
<Call> $args $fn $parent
data.to.allyrs("alfalfa.imperial", "Alfalfa") 
```
These are 8 calls to data.to.allyrs. This function
reads 7 different



```
funs = getDefinedFuns(kk2)
names(funs)
```

```
e2 = findReadDataCalls(funs$data.to.allyrs)
```
```
[[1]]
<Call> $args $fn $parent
read.csv("cropscape_legend.txt", stringsAsFactors = FALSE) 


[[2]]
<Call> $args $fn $parent
read.csv("PRISM.precip.data.updated9.13.17.csv", stringsAsFactors = FALSE) 


[[3]]
<Call> $args $fn $parent
read.csv("SpatialCIMIS.ETo.updated9.13.17.csv", stringsAsFactors = FALSE) 


[[4]]
<Call> $args $fn $parent
read.csv("SpatialCIMIS.U2.updated9.13.17.csv", stringsAsFactors = F) 


[[5]]
<Call> $args $fn $parent
read.csv("SpatialCIMIS.RHmin.updated9.13.17.csv", stringsAsFactors = F) 


[[6]]
<Call> $args $fn $parent
read.csv("cell_counts.csv", stringsAsFactors = FALSE) 


[[7]]
<Call> $args $fn $parent
read.csv("P.WY.summary.10.6.17.csv", stringsAsFactors = FALSE) 


[[8]]
<Call> $args $fn $parent
read.csv(list.files(pattern = glob2rx("*_model_metadata.csv")), 
    stringsAsFactors = FALSE) 


[[9]]
<Call> $args $fn $parent
read.csv(fnames[i], stringsAsFactors = FALSE) 
```

We can see that there are 7 calls to read.csv with fixed/literal file names.
The last two calls to read.csv() are computed and are call-specific.

Since there are 8 calls to this data.to.allyrs function, and it reads these
7 CSV files each time, we can certainly read them once and pass them to the 
8 calls to data.to.allyrs.

It is possible that these 8 calls to read.csv() are conditional.
Let's write a little function that gets the classes of the ancestor nodes in the AST
(up to the Function)
```
ancestorASTClass = 
function(x, reverse = TRUE, stopAtFunctionDef = TRUE) {
  ans = class(x)[1]
  while(!is.null(x <- x$parent)) {
     ans = c(ans, class(x)[1])
	 if(stopAtFunctionDef && is(x, "Function"))
       break
  }
  if(reverse)
    rev(ans)
  else
    ans
}
```
```
sapply(e2[1:7], ancestorASTClass)
```
```
     [,1]       [,2]       [,3]       [,4]       [,5]       [,6]       [,7]      
[1,] "Function" "Function" "Function" "Function" "Function" "Function" "Function"
[2,] "Brace"    "Brace"    "Brace"    "Brace"    "Brace"    "Brace"    "Brace"   
[3,] "Assign"   "Assign"   "Assign"   "Assign"   "Assign"   "Assign"   "Assign"  
[4,] "Call"     "Call"     "Call"     "Call"     "Call"     "Call"     "Call"    
```
So these are simple assignments at the top-level of the function's body (the Brace).
