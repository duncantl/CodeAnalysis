## Introduction

R makes interactive workflows fun and easy. For example, when analyzing
data we might take the following steps:

- Load the data into the workspace
- Plot various slices
- Check for the presence of NA's
- Filter down to a subset of the rows
- Apply a statistical test
- Define a function to apply to many subgroups
- ...

By the end of the process we've learned something about our data, and we
may want to share or preserve some of what we've done. But all we currently
have is an ad hoc, organic data analysis script or command history.

Often this first draft of the code is not idiomatic or efficient. An
experienced R user may be able to quickly go through and improve it, but it
would be better if the code "just fixed itself". R has built-in tools to
generate, inspect, and transform R code. This is sometimes called
metaprogramming or computing on the language. Colloquially, metaprogramming
means "programs which write programs" and treats code as data. Code analysis
and metaprogramming can help us build tools to automatically improve code.

Metaprogramming has received relatively little attention within the R
community. One exception is the __lintr__ package, which checks code for
stylistic and semantic issues. __lintr__ is an example of successful and
practical use of metaprogramming. While __lintr__ only analyzes code, we can go
further by actually modifying the code programmatically. At present, few
packages are available to do this.

There are several cases where metaprogramming is useful:

<!--
*   Writing code is an incremental process. An R script may go through several
    drafts before it is "finished", and even then, it may need to be modified
    in the future to fix bugs or add features.
-->

*   Learning language idioms takes time. Novice users often write code that
    exhibits bad practices, or at best, is not idiomatic. The emphasis on
    vectorization and functional programming in R is especially foreign to
    users coming from other programming languages.

*   Becoming familiar with code that has already been written is challenging.
    Large projects may have multiple contributors and have code and data spread
    across multiple files.

*   Code is not always as expressive as we would like it to be. Programmers are
    good at understanding the high-level intent of their scripts, but typically
    struggle with keeping track of the low-level details. The strengths of the
    computer are exactly the opposite. Low-level details should be left to the
    computer to handle automatically.

Some of these are explored in the examples in this paper in order to draw
attention to how metaprogramming can be used to benefit programmers. These
examples are intentionally limited in depth. We hope the examples will motivate
the development of a richer set of tools for metaprogramming. Potential
developers are the intended readers of this paper.

Code analysis is not a magic bullet. We can't anticipate all ways that one
might misuse R's computational model, but we can detect and improve some of
the common ones. We can use these techniques to understand our code, to
simplify it, and to improve the performance.



## Related Work

Bengtsson's packages, __globals__ and __futures__, use static code analysis
to identify global variables to send to another R session for asynchronous
parallel evaluation.

Bohringer's __parallelize.dynamic__ dynamically decides where to
parallelize code based on the use of potentially nested `Apply()` functions.

Hester's __lintr__ accounts for the most popular use of code analysis
currently in the community. It checks for stylistic consistency. More
relevant to this paper, it identifies unused local variables and probably
mistaken use of global variables.

The __covr__ package checks which fraction package code the unit tests
exercise. It works by walking the AST and inserting tracing code.

Landau's __drake__ offers reproducible computatation on R objects similar to
GNU Make. It detects when variables are updated, and hence trigger more
computations.



trackr
tidyverse NSE
compiler
roxygen


Roxygen2 generates R documentation files based on comments in the code, but
doesn't examine the code itself.
