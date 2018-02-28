
# Questions We Might Answer with Code Analysis

Are two tensors conformable? More generally, what is the shape of a tensor? The
latter is useful for understanding the data structures being manipulated in a
block of code. For instance, if we have code
```r
x = rnorm(n)        # length(x) == n
x = x %o% x         # dim(x) == c(n, n)
x = x[, 1:p]        # dim(x) == c(n, p)
                    #
y = runif(p * n)    # length(y) == p * n
y = matrix(y, p, n) # dim(y) == c(p, n)
                    #
z = x %*% y         # dim(z) == c(n, n)
```
we'd like to keep track of the dimensions of `x`, `y`, `z` at each line. We
need to do this symbolically, so that we can use all of the information the
code provides.

We can use the same information to identify cases where `ifelse()` is called
with a scalar as its first argument.

---

Which uses of `[` drop dimensions? The default of dropping dimensions is a
common source of bugs even for experienced R users.

---

Which calls to apply functions return elements that all have the same type/size
and can be replaced by calls to `vapply()`?

---

Does a vector contain missing values? One way this is useful is in eliminating
unnecessary calls to `which()`. The `which()` function drops missing values, so
a valid use is
```r
x = c(1, NA, 3, 4)

x[which(x > 2)] # More explicit: `x[!is.na(x) & x > 2]`
```
On the other hand, if the condition `x > 2` does not contain any missing
values, there is no reason to use `which()` here and the computation can be
written more efficiently as
```r
x = c(1, 2, 3, 4)

x[x > 2]
```
Overuse of `which()` is common among new R users.

---

When is it safe to remove a variable?

---

When is it safe to remove (or never compute) part of a data structure? For
example, in the code
```r
my_data = data.frame(x = sin(1:3), y = 1:3)

result = my_data$y ^ 2
```
the `x` column of `my_data` is never used. So we don't really need to evaluate
the call `sin(1:3)` or create the column `x`. In this case we could also
replace `my_data` with a vector (a strength reduction), but that can be thought
of as a separate optimization.

---

When can we replace index-by-name with index-by-position? In other words, can
we keep track of the names? We want to do this symbolically. In the example
```r
my_data = data.frame(x = 1:3, y = 1:3) # {"x": 1, "y": 2}
                                       #
names(my_data)[n] = "z"                # {"z": n}
my_data["z"] = 1                       # replace with `my_data[n] = 1`
                                       #
my_data["x"] = 2:4                     # {"x": 1, "z": 2} XOR {"z": 1, "y": 2, "x": 3}
```
the goal is to replace the names in the index operations with positions. We can
do this for `"z"`, but not for `"x"`.

---

Are there any branches of code that never run?

---

Are there any lines of code that _could_ produce a type error?

---

What are the explicit constraints a function developer has placed on the
parameters? For example, in
```r
only_positive = function(x) {
  if (x < 0)
    stop("This function only accepts positive numbers.")

  x + 3
}
```
we would like to recognize that `x >= 0` must hold after the if-statement, and
that anyone calling `only_positive` must pass a positive argument.

---

Which parameters are required by a function and which are optional? In other
words, which arguments will be evaluated?

---

At what line is each argument to a function first evaluated? 

---

Which calls to generic functions can be replaced with calls to specific
methods? This avoids the cost of run-time dispatch, but we must make sure that
the argument to the call always has the same class. This has a real impact on
performance, even for interpreted code. In the example
```r
library(microbenchmark)
x = factor(1:20)

microbenchmark(
  dispatch    = as.character(x),
  no_dispatch = as.character.factor(x)
)
# Setup: Intel Core i7-5500U @ 2.4GHz / 12GB RAM / Linux 4.15.3
## Unit: nanoseconds
##         expr  min     lq    mean median     uq  max neval
##     dispatch 1004 1132.0 1298.45 1196.5 1309.0 7314   100
##  no_dispatch  433  566.5  707.11  617.5  679.5 8459   100
```
calling the method directly is about 1.8 times faster. The speedup will be
smaller for methods that take longer to run.

---

Are some calls redundant? To be more precise, are any pure functions called
multiple times with the same arguments? This is a common mistake for new
programmers. For instance, we could write
```r
x = matrix(runif(20), 10, 2)

scale(x)[scale(x)[, 1] > 0, ]
```
Since `scale()` is a pure function, it is better to write
```r
scaled = scale(x)
scaled[scaled[, 1] > 0, ]
```
Closely related are calls that can be lifted out of loops.

---

Which functions are pure? This is useful information for the question above,
but also useful for other optimizations that involve changing the order of
code.

---

Which loops or applies can be fused?

---

Which loops or applies have iterations that can be run in parallel?

---

Which loops/applies can be translated into vectorized operations?
```r
x = runif(10)
for (i in seq_along(x))
  x[i] = sin(x[i])

# OR:
x = sapply(x, sin)
```

---

Which loops can be translated into applies?

---

Which loops should be using preallocation but aren't?
```r
x = 7                      # length(x) == 1
for (i in 1:n) {           #
  x[i + 1] = x[i] + x[1]^2 #
}                          # length(x) == n + 1
```
We can use heuristics to detect this (cf. Clark's work) but more generally we
are looking for cases where a variable's shape differs at the entry and exit of
a loop.



## Notes from Meeting with Duncan

What kinds of information do we want to extract from code?

Consider the code

```r
test = function(f) {
  list(f("3"), f(2))
}
```

Rather than getting stuck on this code, we can note that `f` should accept both
strings and numerics as its argument.

If a separate tool for static checking is used after analysis, it can warn the
user that there is ambiguity about the type of `f`.

---

Consider the code
```r
x = list()          # length(x) == 0
for (i in 1:p) {    #
  x[[i]] = i        #
}                   # length(x) == p
                    #
foo(x[[1]], x[[2]]) # length(x) >= 2
```

Unless we know `p`, we can't tell whether `x[[1]]` and `x[[2]]` actually exist.
We can however note that `p >= 2` is necessary for the call to `foo()` to
succeed.

On the other hand, if we see
```r
x = list()              # names(x) == NULL
for (i in 1:p) {        #
  x[[i]] = i            #
}                       #
                        #
foo(x[["a"]], x[["b"]]) # c("a", "b") %in% names(x)
```

Then we can note `x` must have elements `"a"` and `"b"`. Since `x` is defined
in this code snippet and names are never set, we can be fairly sure that the
call to `foo()` will fail.

---

More generally, we'd like to extract symbolic vector/list bounds. For instance:

```r
x = c(1, 2, 3) # length(x) == 3
y = c(4, 5)    # length(y) == 2
z = x %o% y    # dim(z) == c(3, 2)
a = z[1, ]     # length(a) == 2
```

Understanding the shapes of data in the code helps us understand what the code
is actually doing.
