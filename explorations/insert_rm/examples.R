
# ------------------------------------------------------------
# Example 1 - No Branches
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

# ------------------------------------------------------------
# Example 2 - If Statement
example2 = function() {
  x = 0
  if (rnorm(1) > 0) {
    y = x
  } else {
    y = x + 1
  }
  z = y
}

# ------------------------------------------------------------
# Example 3 - For Loop
example3 = function() {
  x = 0
  for (i in 1:3) {
    x = x + i
  }

  y = x

  bar(y)
}

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
