# Ideas for Code Analysis

TODO (20 June 18):

- [Clark and Nick] See what Rstudio refactoring tools do.
- [Matt and Duncan] will write the skeleton and decide initially what features the  paper should include.
- Criteria for inclusion: Maximally useful and fit into Nick and Clark's theses (to avoid work that
  doesn't help them finish).
- List other static analysis tasks that are common / useful in other languages.

Goals for the paper:

- Refactoring, making things more clear, succint
- Not changing mode of evaluation

Aim for September 2018 to submit this.

TODO: Write more about the examples. Why is this cool / interesting? Can we
explain it in a conversational way? Even before writing code.

Organizational: We can split these up into sub directories for each task.
Use plain text.

Possible Running Examples:
1. Matt's yield gap analysis project
1. Scott's code (data not available)


### TOP

1. [partially done] [Nick thesis] Identify when a variable can be
   rm()'ed (since no longer used) and so garbage collected. See CodeDepends
   for this.
1. [Nick thesis] Replace identical calls with variables so you don't
   compute the same thing multiple times.
1. [Nick / Duncan] Dead code removal - remove expressions which are
   not used later; whose result is not used). Recursing into functions to
   identify possible side effects.
1. [partially done] [Clark thesis / Duncan] Loop "correction" that lacks
   preallocation See explorations/findConcat.R and explorations/concat.R
   example.  Identify and rewrite.
2. [TOP] [Clark / Duncan] Map code inside for loops into apply() -- simple
   examples. Check RLoopFusion.
1. ?(maybe better in different package?)[TOP] [Clark] Opportunities for parallelization, for example replacing
   lapply with parallel::mclapply in an intelligent way to avoid nesting.
1. [TOP] [Matt / Duncan] Identify input/output data files (see CodeDepends).
     + See CodeAnalysis::getInputFiles(), getOutputFiles(), getGraphicsOutputFiles()
1. [TOP] [Matt / Duncan] Summarize code from a project (see RCleanProject).
   Find minimal example: create graph of scripts for directory?
1. Identify repetitive blocks of code, i.e. the same code evaluated in multiple
   places with just one argument changed. Find expressions that differ only
   by one term and that look like they should be in a loop.
1. [see getGlobals() lot done] findGlobals() alternative that handles, e.g.,  pkg::fun as a 
   single entity and not ::, pkg, fun.
   See explorations/findGlobals.R.  (Nick: also done in rstatic, see
   [here](https://github.com/nick-ulle/rstatic/blob/master/R/collapse_namespaces.R))
   and also recognizes fun in lapply(x, fun)  as  function and not a
   variable. Find use of undefined variables.
   CodeDepends:::freeVariables(readScript(file))
     + CodeAnalysis::getGlobals() handles pkg::fun and lapply() and other indirect calls, e.g., do.call()


### Improvements

Improve the general well being of the R session, and may improve clarity.

1. Identify invariants that are recomputed, e.g. within loops, or in
   multiple expressions.
1. Check for possible subclass relationships among S3 objects.
2. [Clark] Identify code which can be safely evaluated during static
   analysis, ie. `c(1:2, 4:6)`. Example using this information for another part
   of analysis.
1. Make `Vectorize()` recognize when everything in it is vectorized and
   then don't change it into mapply. Related- identify calls to vectorized
   functions in calls to apply family.


### Data Related

1. [DONE] Identify unused columns in a data frame read via
   read.table()/etc. so that we can add colClasses = NULL for these. The
   code for this is now here in the package, see `R/readFaster.R`.
   Also colClasses and type inference.
1. [Nick / Duncan] Check whether fields in S3 object exist or are defined.
1. Validation of slots and fields in S4, Ref classes, R6
1. [MED] change sapply -> vapply or lapply based on type and dimension
   inference.
1. Identify objects that are supposed to have the same length.
1. [Nick (probably not in this paper)] General object dimension inference
1. Identify when `x[, , drop = TRUE]` will produce result of different type
   than x.


### Functions

1.  (see functionDivision) Refactoring functions to smaller, more modular functions. Detect blocks
   of expressions that have direct data dependence and make these into
   functions.
1. [LAST] [Nick] Extract functions defined inside functions that do not modify
   the shared variables. To allow testing.
1. [DONE Duncan] Later, grow our findGlobals() to  a) descend into nested function definitions and thus handle
   closures correctly, b) identify variables that are used before being defined.
2. [?DONE Duncan] Rewrite functions to make explicit dependence on global variables, ie:
```{R}
f = function(x) x + y
# becomes:
f = function(x, .y = y) x + .y
```
1. [MED] Identify self-referencing parameters, e.g. 
```
function(x = length(y), y = sum(x))
```
and strict/lazy eval
1. Identify recursive functions so can change name throughout if change name of top-level function.
```{r}
fns = getFunctionDefs("R")  # CodeAnalysis package
isRecursive = sapply(names(fns), function(id) id %in% getGlobals(fns[[id]])$functions)
table(isRecursive)
```
1. Infer where parameters might be evaluated.
1. Change all instances of a name in a package to another name. For
   example, `foo` is the name of a function and a parameter. Very difficult
   with regex.
1. Identify probable mistakes in S3/S4 method definitions. For example, a
   package defines a class "MyWonderfulClass" and then goes on to define a
   method for "MeWonderfulClass".
1. Visualizing functions with clickable explorable connections.


### General & Packaging

1. [MED] Identify checkpoints where intermediate results can be saved to resume
   later (see
   [Drake](https://cran.r-project.org/web/packages/drake/vignettes/drake.html)).
1. Extend CodeDepends to handle S4 methods.
1. ?Documentation generation?
1. Global changes between coding naming conventions, i.e. `snake_case` and `CamelCase`.
   This requires some care to avoid writing over existing variable names.
