# CRAN Survey

## Motivation

* How commonly used are certain features of R? These could be any features
  we're interested in. E.g., assignment from a conditional.
* How complex is the code? E.g., number of calls inside a function
* How has the complexity evolved over time?


## Package Statistics Wishlist

Per function:

*   depth of nesting:
    +   control flow
    +   calls
    +   parentheses

*   total number (or locations) of:
    + calls
    + calls, including calls made by calls (computed recursively)
    + literals
    + globals
    + loops, broken down by `for`, `while`, apply family
    + number of parameters (and whether default arguments are calls, other
      parameters, literals)
    + `<<-` assignments
    + calls to NSE/macro functions: `eval()`, `substitute()`, `quote()`, ...
    + calls to foreign functions: `.C()`, `.Call()`
    + comments and number of characters
    * namespacing: `::` and `:::`, also use `NAMESPACE` to identify calls to
      functions in other packages that don't use `::`
    * conditional assignments
    * calls to parallelism functions, such as from __parallel__

* Whether a function can be parallelized?
* Whether a function always returns the same type?

Per package:

*   number of
    * exported objects
    * private objects
    * object-oriented classes and methods (S3, S4, RefClasses)

* What are all the dependencies of this package?
* Which of the non exported functions can be called by exported functions?
  (recursively - identifies a type of dead code)
* "Amount" of R code versus C/C++/Fortran code
* Metadata from `DESCRIPTION` (e.g., author, date, versions)
* Associate author names to git blame
