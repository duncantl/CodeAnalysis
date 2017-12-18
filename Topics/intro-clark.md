Clark writing this:

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
would be better if the code "just fixed itself". This is where code
analysis and metaprogramming can help. 

This paper demonstrates R code analysis techniques through the following
examples: By examining a collection of related R files that may `source()`
each other and create other files such as plots and data we can determine
the structure of new projects, or of old projects that we've forgotten.
Next we show an example of programmatically changing existing code from a
slow, non idiomatic `for` loop into a parallel `lapply`. The next example
shows several ways to simplify code to make it easier to understand. The
last example actually improves performance by using less memory.

Code analysis is not a magic bullet. We can't anticipate all ways that one
might misuse R's computational model, but we can detect and improve some of
the common ones.  Colloquially, metaprogramming means "programs which write
programs". We can use these techniques to understand our code, to simplify
it, and to improve the performance.


## Related Work
