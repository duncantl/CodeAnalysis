# CRAN Survey

## Motivation

* How commonly used are certain features of R? These could be any features
  we're interested in. E.g., assignment from a conditional.
* How complex is the code? E.g., number of calls inside a function
* How has the complexity evolved over time? Need a time series of code
* How does the evolution of packages on CRAN compare to other repositories
  like CPAN, CTAN, pip, etc?


## Package Statistics Wishlist

Per function:

*   depth of nesting:
    +   control flow
    +   calls
    +   parentheses

*   total number (or locations) of:
    * local aliases for global functions (e.g., `options()`, `rm()`)
    + calls
    + calls, including calls made by calls (computed recursively)
    + ratio of calls inside packages compared to base, recommended, and 3rd
      party packages.
    + literals
    + globals
    + loops, broken down by `for`, `while`, apply family
    + Depth of nested loops (computed recursively)
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
    * Reference objects such as environments, closures, and formulas. Do
      people use these directly?

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
* Which exported functions are used in the vignette?
* Which packages use Roxygen? Is there more or less documentation compared
  to Rd files?
* Usage of specific libraries over time, ie. Rcpp and magrittr `%>%`.
* Usage of plotting libraries over time, ie. base, grid, lattice, ggplot
* Presence of C/C++/Fortran/Java/other language code
* "Amount" of R code versus foreign language code
* Metadata from `DESCRIPTION` (e.g., author, date, versions)
* Associate author names to git blame
* Usage of system C libraries

Ideas for data sources:

* Stack Overflow
* R user mailing lists
