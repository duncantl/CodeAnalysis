# Ideas for Code Analysis

Goals for the paper:

Keep the examples simple and manageable.

Aim for December 21st, 2017 to submit this.

TODO: Write more about the examples. Why is this cool / interesting? Can we
explain it in a conversational way? Even before writing code.

Organizational: We can split these up into sub directories for each task.
Use plain text.

Possible Running Examples:
1. Matt's yield gap analysis project
1. Scott's code (data not available)


### Performance

1. Identify repetitive code, i.e. the same code evaluated in multiple
   places.
1. [TOP partially done] [Nick / Duncan] Identify when a variable can be
   rm()'ed (since no longer used) and so garbage collected. See CodeDepends
   for this.
1. [TOP] [Nick / Duncan] (remove redudant code)
1. [TOP] [Nick / Duncan] Dead code removal - remove expressions which are
   not used later; whose result is not used. Recursing into functions to
   identify possible side effects.
1. Find use of undefined variables.
   CodeDepends:::freeVariables(readScript(file))
1. Identify invariants that are recomputed, e.g. within loops, or in
   multiple expressions.
1. Find expressions that differ only by one term and that look like they
   should be in a loop.
1. [TOP partially done] [Clark / Duncan] Loop "correction" that lacks
   preallocation See explorations/findConcat.R and explorations/concat.R
   example.  Identify and rewrite.
2. [TOP] [Clark / Duncan] Map code inside for loops into apply() -- simple
   examples. Check RLoopFusion.
1. [TOP] [Clark] Opportunities for parallelization -- simple examples

### Data Related

1. [DONE] Identify unused columns in a data frame read via
   read.table()/etc. so that we can add colClasses = NULL for these. The
   code for this is now here in the package, see `R/readFaster.R`.
1. [MED] colClasses and type inference -- `vapply()`
1. Identify objects that are supposed to have the same length.

### Functions

1. Refactoring functions to smaller, more modular functions
1. [LAST] [Nick] Extract functions defined inside functions that do not modify
   the shared variables. To allow testing.
1. [basics done] findGlobals() alternative that handles, e.g.,  pkg::fun as a 
   single entity and not ::, pkg, fun.
   See explorations/findGlobals.R.  (Nick: also done in rstatic, see
   [here](https://github.com/nick-ulle/rstatic/blob/master/R/collapse_namespaces.R))
   and also recognizes fun in lapply(x, fun)  as  function and not a variable.
1. Later, grow our findGlobals() to  a) descend into nested function definitions and thus handle
   closures correctly, b) identify variables that are used before being defined.
2. Rewrite functions to make explicit dependence on global variables, ie:
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

### General & Packaging

1. [TOP] [Matt / Duncan] Identify input/output data files (see CodeDepends).
1. [TOP] [Matt / Duncan] Summarize code from a project (see RCleanProject).
   Find minimal example: create graph of scripts for directory?
1. [MED] Identify checkpoints where intermediate results can be saved to resume
   later (see
     [Drake](https://cran.r-project.org/web/packages/drake/vignettes/drake.html)).
1. CodeDepends and processing S4 methods.
1. Ref classes and S6 and validation.
1. ?Documentation generation?
2. [TOP] [Clark] Identify code which can be safely evaluated during static
   analysis, ie. `c(1:2, 4:6)`. Example using this information for another part
   of analysis.

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
