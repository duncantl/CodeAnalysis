# Ideas for Code Analysis with rstatic, etc.

1. Identify repetitive code, i.e. the same code evaluated in multiple places.
1. Identify when a variable can be rm()'ed (since no longer used) and so garbage collected
1. Remove redudant code, i.e. whose result is not used.
1. Loop "correction" that lacks preallocation, or that can be mapped to apply()
1. Identify unused columns in a data frame read via read.table()/etc. so that we can
   add colClasses = NULL for these
1. colClasses and type inference.
1. Refactoring functions to smaller, more modular functions
1. Extract functions defined inside functions that do not modify the shared variables.
1. Summarize code from a project (see RCleanProject)
1. findGlobals() alternative that handles, e.g.,  pkg::fun as a single entity and not ::, pkg, fun
   and also recognizes fun in lapply(x, fun)  as  function and not a variable.
1. CodeDepends and processing methods.
1. Ref classes and S6 and validation.

