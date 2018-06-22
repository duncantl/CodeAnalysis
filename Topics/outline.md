## Abstract

In this paper we demonstrate new opportunities to apply code analysis to the R
language. Many of these have not been explored before by the R community. These
analyses can help us both understand our R code better and improve the
performance.

We hope the prototypes we present here will motivate members of the community
to develop a more robust set of tools for code analysis.

## Introduction

* R workflow is interactive. We go through multiple drafts of programs. Often
  the early drafts are not idiomatic or efficient. We can use code analysis to
  make it easier to clean them up.
  
  1. At the most basic level, code analysis can help us understand a package's/project's structure, dependencies, etc.
  2. Knowing that structure, we can go a step further to clean the existing code, removing redudencies or unused pieces
  3. But we are not just limited to cleaning the code. We can also refactor the code to be more efficient, or to take advantage of opportunities for increased performance, e.g. parallelization.
  
## Getting Information About Project Structure

* Projects tend to build up a lot of files/functions over time.  Even
  well-documented projects can be difficult to understand and navegate
  function and file dependencies and relationships.  
  This is especially the case in long-running projects or projects
  worked on by many people.

1. Identify input/output data files (see CodeDepends).
    * Useful to typical useRs.
2. Identify function dependencies, entry points, and wrapper functions
3. Summarize code from a project (see RCleanProject).
   Find minimal example: create graph of scripts for directory?
    * Richer example of what can be done by tool developers.
4. Aids in creation of build files, e.g. GNU make

## Cleaning Up Code

By cleaning up code we mean removing code that is not needed, putting code in
more idiomatic forms, and also avoiding unecessary computation. This reduces
memory use and running time. This also makes the code base smaller and more
consistent, so it is easier to understand and maintain.

Code formatters have a similar effect on consistency and readability,
but here we are actually transforming code. In other words, we are not
merely concerned with altering the appearance of the code, but
altering the function.  This goes beyond what a code formatter can do,
as it may actually improve performance.

### Loop to parallel apply

1. Loop "correction" that lacks
   preallocation See explorations/findConcat.R and explorations/concat.R
   example.  Identify and rewrite.
2. Map code inside for loops into apply() -- simple
   examples. Check RLoopFusion.
3. Make `lapply()` run in parallel
* Parallelism is hard. The functional style of R makes it particularly
  well-suited to automatically running code in parallel. We can transform code
  and run it in parallel without users needing to understand the complexities
  of parallelism, or even needing to learn a new package.

### Making Code Easier to Understand

* Sometimes we may want to keep our transformations to code in place. In other
  words, the transformation improves the quality of the code so we save it in a
  script. On the other hand, some transformations improve performance but not
  necessarily readability. These do not necessarily need to be saved for the
  user, but can just be applied before the code runs. This is suggestive of
  compilation or transpilation.

1. Extract functions defined inside functions that do not modify
   the shared variables. To allow testing.
   See Topics/extractFunctions/.
   (DTL: I needed this for another purpose so implemented it. We had it as LAST, but different
   considerations arose.)
2. Remove expressions which are not used
   later, unless they contain known functions with side effects. A complete
   solution would require recursing into called functions to identify possible
   side effects.
3. Replace redundant expressions with variables.

### Avoid Unnecessary Memory Use

1. Identify when a variable can be
   rm()'ed (since no longer used) and so garbage collected. See CodeDepends
   for this.
2. Identify unused columns in a data frame read via
   read.table()/etc. so that we can add colClasses = NULL for these. The
   code for this is now here in the package, see `R/readFaster.R`.
   * Progress from `d$myvar` to `x = "myvar"; d[[x]]` and beyond. This
     motivates point below.
3. Identify code which can be safely evaluated during static
   analysis, ie. `c(1:2, 4:6)`. Example using this information for another part
   of analysis.
