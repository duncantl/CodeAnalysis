# Ideas for Code Analysis

### Performance

1. Identify repetitive code, i.e. the same code evaluated in multiple places.
1. Identify when a variable can be rm()'ed (since no longer used) and so garbage collected
1. Remove redudant code, i.e. whose result is not used. aka dead code
   removal
1. Identify invariants that are recomputed, e.g. within loops.
1. Loop "correction" that lacks preallocation, or that can be mapped to apply()

### Data Related

1. Identify unused columns in a data frame read via read.table()/etc. so that we can
   add colClasses = NULL for these. (Clark: Here is [code for
this](https://github.com/clarkfitzg/codedoctor/blob/master/R/read_faster.R))
1. colClasses and type inference.
1. Opportunities for parallelization

### Functions

1. Refactoring functions to smaller, more modular functions
1. Extract functions defined inside functions that do not modify the shared variables.
1. findGlobals() alternative that handles, e.g.,  pkg::fun as a single entity and not ::, pkg, fun
   and also recognizes fun in lapply(x, fun)  as  function and not a variable.
2. Rewrite functions to make explicit dependence on global variables, ie:
```{R}
f = function(x) x + y
# becomes:
f = function(x, .y = y) x + .y
```

### General & Packaging

1. Summarize code from a project (see RCleanProject)
1. CodeDepends and processing S4 methods.
1. Ref classes and S6 and validation.
1. ?Documentation generation?
2. Identify code which can be safely evaluated early, ie. `c(1:2, 4:6)`.


## Meeting

To discuss in group meeting this week:

1. Outcomes: paper?, CRAN package?
2. Focus: Who will take an interest? Package developers? More / less
   experienced R users? Maybe different for each application
2. Code analysis package dependencies
    - It's confusing for an outsider if Duncan, Gabe, Nick, and Clark to
      each have their own somewhat related R code analysis package
    - rstatic and CodeDepends are somewhat lower level, perhaps this is an
      opportunity to make something more user facing.
