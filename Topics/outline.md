## Abstract

In this paper we demonstrate new opportunities to apply code analysis
to the R language. Many of these have not been explored before by the
R community. These analyses can help us both understand our R code
better and improve the performance.

We hope the prototypes we present here will motivate members of the
community to develop a more robust set of tools for code analysis.

## Introduction

One of the main benefits of the R programming language is that the R
workflow is interactive. We are free to experiment iteratively,
expanding a function or statistical analysis in pieces. We are able to
develop R programs quickly in this way. Due to this interactive and
iterative nature, R programs tend to go through multiple drafts. The
first draft is often an ad hoc, "organically created" script or even
merely a command history. Early drafts are rarely idiomatic,
consistent, or efficient and many times the method that we found to
work through experimentation is a far cry from best solution. Hence,
R's interactive nature can be a "double-edged sword" when it comes to
creating code that is easy to understand and maintain, efficient, and
high-performance.

Experienced R programmers can manually correct many of these first
drafts, but even for advanced users this process is time-consuming and
error-prone. Additionally, as code becomes more efficient and
higher-preformance, it often becomes more difficult for humans to read
and understand. Ideally, we want like to preserve the interactive and
iterative process of creating code in R, along with code which is easy
for humans to read and understand.  We would also like to automate the
process of cleaning that code and refactoring it to take advantage of
high-performance and efficiency constructs.

In this paper, we demonstrate how we can use code analysis achieve
these goals:

  1. At the most basic level, code analysis can help us understand a
     package's/project's structure, dependencies, etc.
  2. Knowing that structure, we can go a step further to clean the
     existing code, removing redudencies or unused pieces
  3. But we are not just limited to cleaning the code. We can also
     refactor the code to be more efficient, or to take advantage of
     opportunities for increased performance, e.g. parallelization.

To allow others to quickly and easily utilize the techniques
demonstrated here, we have also created an R package. We hope that
this paper and package will serve as motivation for others to explore
the potential code analysis has.

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

### Making Code Easier to Understand

Generally, as code grows over time it becomes more difficult to
understand for reasons that are not directly due to an increase in
functional complexity.  
The proliferation of unused variables and functions can
clutter up code, making it difficult to read and understand. Likewise,
code which is duplicated or redundant impeded our ability to
understand the code base by obscuring the relevant pieces.  Large
functions definitions, which often expand over time to include
functions defined inside of other functions, can make it difficult to
break down and test individual steps of a process.  Yet correcting
these issues by hand is tedious, time-consuming, and error-prone.
Therefore, we would like to be able to programmatically:

1. Remove expressions which are not used
   later, unless they contain known functions with side effects. A complete
   solution would require recursing into called functions to identify possible
   side effects.
2. Replace redundant expressions with variables.
3. Extract functions defined inside functions that do not modify
   the shared variables. To allow testing.
   See Topics/extractFunctions/.
   (DTL: I needed this for another purpose so implemented it. We had it as LAST, but different
   considerations arose.)

* Sometimes we may want to keep our transformations to code in place. In other
  words, the transformation improves the quality of the code so we save it in a
  script. On the other hand, some transformations improve performance but not
  necessarily readability. These do not necessarily need to be saved for the
  user, but can just be applied before the code runs. This is suggestive of
  compilation or transpilation.

## Refactoring code

Refactoring code involves changing the implementation of the code to make the code more
efficient or higher performance without changing the code's purpose.
This is a step beyond merely understand the structure of a code base or cleaning that
code base to make it easier to understand and work with.

This can be thought of changing the code's structure without changing its function.

### Avoid Unnecessary Memory Use

It is well known that many R programs or functions can become memory
limited due to the fact that R holds objects in memory. In some cases,
the memory requirements can exceed the memory available, which leads
to a large performance cost as objects are moved into and out of swap
memory on the disk. Hence, reducing the memory requirements of an R
script or function can have direct performance benefits.

Broadly, there are three ways that we can reduce the memory requirements of R code:

1. Free up used memory when it is no longer needed: This involves
   identify where in the process a variable is no longer used and can
   be safely rm()'ed from the environment. Once the object is removed,
   garbage collection will free the memory.  See CodeDepends for this.
2. Avoid using memory in the first place: Identify unused columns in a data frame read via
   read.table()/etc. so that we can add colClasses = NULL for these. The
   code for this is now here in the package, see `R/readFaster.R`.
   * Progress from `d$myvar` to `x = "myvar"; d[[x]]` and beyond. This
     motivates point below.
3. Identify code which can be safely evaluated during static
   analysis once, ie. `c(1:2, 4:6)`. Example using this information for another part
   of analysis.
   
### Loop to parallel apply

In R, `for` loops are easy to understand but, unlike most compiled
languages, computationally expensive. Ideally, we would be able to
write a function using the simple `for` construct, but then convert
this into a more efficient implementation utilizing R's built-in apply
family of functions.  Not only can this increase efficiency, once
refactored into one of these apply functions, it then becomes
relatively trivial to convert the function to be run across parallel
processes for increased performance.

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
