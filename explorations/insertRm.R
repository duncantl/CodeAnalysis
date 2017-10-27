# This script has examples and code of inserting `rm()` after a variable stops
# being live. In other words, after the last time a variable is used, we can
# call `rm()` for the variable so that the memory will be freed on the next
# garbage collector run.
#
# For example, if we have some code

example1 = function() {
  x = 3
  y = x
  z = x

  # ...
  bar(y)
}

# then we can insert an `rm(x)` after the line `z = x` since `x` is never used
# again in the function.
#
# First we can try to do this using CodeDepends.

library(CodeDepends)

# Does `getInputs()` not work with a user-defined function as the argument?

# info = getInputs(example1) # Error

# Seems to work okay with built-in functions, e.g., `getInputs(lm)`.


expr = parse(text =
  "example1 = function() {
    x = 3
    y = x
    z = x
  
    # ...
    bar(y)
  }"
)

# No error but doesn't seem to analyze internal code.
info = getInputs(expr)

# tl = getDetailedTimelines(info = info) # Error

sc = new("Script", as.list(body(example1)[-1]))
info = getInputs(sc)

get_last_uses = function(script, info) {
  tl = getDetailedTimelines(script, info = info)

  by_var = split(tl, tl$var)

  vapply(by_var, function(var) {
    last_use = tail(which(var$used), 1)
    if (length(last_use) == 0) {
      # Never used.
      warning(sprintf("Variable '%s' is never used.", var$var[[1]]))
      return (NA)
    }
      
    var$step[[last_use]]
  }, NA_integer_)
}

last_uses = get_last_uses(sc, info)

# Now insert `rm()` after each last use, working backward from the bottom of
# the code so as not to mess up the order.
insert_rm = function(script, last_uses) {
  # Don't do anything for variables that are never used. But we could also
  # remove them.
  last_uses = sort(last_uses, na.last = NA, decreasing = TRUE)

  Map(function(name, last_use) {
    rm_code = call("rm", as.name(name))
    script <<- append(script, rm_code, last_use)
    NULL
  }, names(last_uses), last_uses)

  script
}

result1 = insert_rm(sc, last_uses)

# Let's try a more complicated example with a for-loop.
example2 = function() {
  x = 0
  for (i in 1:3) {
    x = x + i
  }

  y = x

  bar(y)
}

sc2 = new("Script", as.list(body(example2)[-1]))
info2 = getInputs(sc2)

last_uses2 = get_last_uses(sc2, info2)

# This example shows that we need to extend get_last_uses() to recurse into the
# for-loop because `getDetailedTimelines()` does not by default. More
# generally, it needs to recurse into all control structures.
#
# This means the code to insert `rm()`s also has to be recursive. All of the
# last uses have to be collected first, though, because a variable such as `i`
# could be used above the control structure where it's defined.


# ------------------------------------------------------------
# Now let's try this with rstatic.

library(rstatic)

node1 = to_cfg(example1)

plot(node1$ssa)

# Need a better way to get info for specific variables from the SSA graph.
#
# All of the info we need is there, but it's not easy to get at. We also need
# the basenames of all variables in the AST.
#
# Here's a hacky way to do it:

vars = names(node1$ssa$blocks)
vars = vars[!grepl("^(%|[.]_)", vars)]

first_def = vars[grepl("_1$", vars)]

# For each variable, find the node immediately after its last definition.
#lapply(first_def, function(name) {
#  uses = adjacent_vertices(node1$ssa$graph, name, "out")
#
#  # When there's more than one use, there's no easy way to tell which use comes
#  # first in the code.
#  browser()
#})


# ------------------------------------------------------------
# "Real" example
dist2means = function(x) {
    # This function computes the distance from
    # between a vector x and each of the means.
    apply(means, 1, function(m) norm(x - m, '2'))
}

k_means = function(data, k, max_iter = 10,
                   interactive = TRUE, tol = 0.0001)
    # This function performs an interactive version of
    # the k-means algorithm.
{
    # Convert data to a matrix.
    data = as.matrix(data)
    n = nrow(data)

    # 0. Choose k observations randomly as the initial
    # cluster means.
    start = sample(n, k)
    means = data[start, ]

    for (iter in seq_len(max_iter)) {
        # 1. Cluster each observation according to closest mean.
        scores = apply(data, 1, dist2means)
        scores = -t(scores)
        clusters = max.col(scores)

        # 2. Recompute the mean for each cluster.
        old_means = means
        for (i in seq_len(k)) {
            means[i, ] = colMeans(data[clusters == i, ])
        }

        # 3. Check for convergence.
        dist = norm(means - old_means, '2')
        if (dist < tol) {
            cat(paste0(dist, ' is within tolerance.\n'))
            return(clusters)
        }
    } # end for
    return(clusters)
}
