## Introduction

One of the main benefits of the R programming language [@R] is that the R
workflow is interactive. We are free to experiment iteratively,
expanding a function or statistical analysis in pieces. We are able to
develop R programs quickly in this way. For example, when analyzing
data we might take the following steps:

- Load the data into the workspace
- Plot various slices
- Check for the presence of NA's
- Filter down to a subset of the rows
- Apply a statistical test
- Define a function to apply to many subgroups
- ...

Due to this interactive and iterative nature, R programs tend to go
through multiple drafts. The first draft is often an ad hoc,
"organically created" script or even merely a command history. Early
drafts are rarely idiomatic, consistent, or efficient and many times
the method that we found to work through experimentation is a far cry
from best solution. Hence, R's interactive nature can be a
"double-edged sword" when it comes to creating code that is easy to
understand and maintain, efficient, and high-performance.

Due to this interactive and
iterative nature, R programs tend to go through multiple drafts. The
first draft is often an ad hoc, "organically created" script or even
merely a command history. Early drafts are rarely idiomatic,
consistent, or efficient and many times the method that we found to
work through experimentation is a far cry from best solution. Hence,
R's interactive nature can be a "double-edged sword" when it comes to
creating code that is easy to understand and maintain, efficient, and
high-performance.

Experienced R programmers can manually correct many of the issues in
these first drafts, but even for advanced users this process is
time-consuming and error-prone. Additionally, as code becomes more
efficient and higher-performance, it often becomes more difficult for
humans to read and understand. This is especially true in R, which
emphasises highly vectorized code.

Ideally, we want like to preserve the interactive and iterative
process of creating code in R which is easy for humans to read and
understand. If we could automate the process of cleaning that code and
then refactoring it to take advantage of high-performance and efficiency
constructs, we would ideally get the best of both worlds.

R has built-in tools to generate, inspect, and transform R code. This
is sometimes called metaprogramming or computing on the
language. Colloquially, metaprogramming means "programs which write
programs" and treats code as data. Code analysis and metaprogramming
can help us build tools to automatically improve code quality and performance.

<!--

Metaprogramming has received relatively little attention within the R
community. One exception is the __lintr__ package, which checks code for
stylistic and semantic issues. __lintr__ is an example of successful and
practical use of metaprogramming. While __lintr__ only analyzes code, we can go
further by actually modifying the code programmatically. At present, few
packages are available to do this.

-->

There are several cases where metaprogramming is useful:

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

In this paper, we demonstrate how we can use code analysis to:

  1. At the most basic level, code analysis can help us understand a
     package's/project's structure, dependencies, etc.
  2. Knowing that structure, we can go a step further to clean the
     existing code, removing redudencies or unused pieces
  3. But we are not just limited to cleaning the code. We can also
     refactor the code to be more efficient, or to take advantage of
     opportunities for increased performance, e.g. parallelization.

Some of these are explored in the examples in this paper in order to
draw attention to how metaprogramming can be used to benefit
programmers. These examples are intentionally limited in depth. We
hope the examples will motivate the development of a richer set of
tools for metaprogramming. Potential developers are the intended
readers of this paper. To allow others to quickly and easily utilize
the techniques demonstrated here, we have also created an R
package. We hope that this paper and package will serve as motivation
for others to explore the potential code analysis has.

Code analysis is not a magic bullet. We can't anticipate all ways that one
might misuse R's computational model, but we can detect and improve some of
the common ones. We can use these techniques to understand our code, to
simplify it, and to improve the performance.

## Related Work

Bengtsson's packages, __globals__ [@globals] and __future__ [@future], use static code analysis
to identify global variables to send to another R session for asynchronous
parallel evaluation.

Bohringer's __parallelize.dynamic__ [@parallelize_dynamic] dynamically decides where to
parallelize code based on the use of potentially nested `Apply()` functions.

Hester's __lintr__ [@lintr] accounts for the most popular use of code analysis
currently in the community. It checks for stylistic consistency. More
relevant to this paper, it identifies unused local variables and probably
mistaken use of global variables.

The __covr__ [@covr] package checks which fraction package code the unit tests
exercise. It works by walking the AST and inserting tracing code.

Landau's __drake__ [@drake] offers reproducible computatation on R objects similar to
GNU Make. It detects when variables are updated, and hence trigger more
computations.

Several packages in the __tidyverse__ [@tidyverse] family use non-standard evaluation to
build a language within the language. Non-standard evaluation means that code
is captured and possibly modified before it is evaluated. The uses of
non-standard evaluation in these packages are relatively simple cases of
metaprogramming.

The __trackr__ [@trackr] package is designed to record information about the artifacts
(data, plots, etc) of a computation so that they can be organized and easily
discovered later. In addition to collecting metadata from other sources,
__trackr__ collects metadata about the computation from the code itself. This
is an example of using metaprogramming to gather information from code about a
project.

The __compiler__ [@R] package is perhaps the best example of how metaprogramming can
be used to radically improve the performance of R code. The package takes R
code and translates it into bytecode, a machine-readable code that can be
interpreted faster than R code. Since the translation from R expressions to
bytecode instructions is not one-to-one, the __compiler__ package has to
implement some logic (or intelligence about R code).

The __roxygen2__ [@roxygen2] package generates R documentation files based on comments in
the code, but doesn't examine the code itself. While __roxygen2__ is not an
example of code analysis, the package does analyze structured comments created
by the programmer. We point it out because code analysis could potentially be
used to automatically collect some of the information that __roxygen2__
currently requires programmers to write manually.
