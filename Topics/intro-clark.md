## Abstract

In this paper we demonstrate new opportunities to apply code analysis to the R
language. Many of these have not been explored before by the R community. These
analyses can help us both understand our R code better and improve the
performance.

We hope the prototypes we present here will motivate members of the community
to develop a more robust set of tools for code analysis.

## Introduction

R makes interactive workflows fun and easy. For example, when analyzing
data we might take the following steps.

- Load the data into the workspace
- Plot various slices
- Check for the presence of NA's
- Filter down to a subset of the rows
- Apply a statistical test
- Define a function to apply to many subgroups
- ... and so on

This results in an ad hoc, organic data analysis script.

Often we start out with global variables and ad hoc organic scripts. Once
we realize that parts of it may be useful elsewhere we might start
organizing the lines of code into functions. Often the early drafts of the
code are not idiomatic or efficient.

We can use code analysis to
  make it easier to clean them up.
* (MORE)


