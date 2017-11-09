# This script has examples and code of using rstatic to insert `rm()` after a
# variable stops being live.
#
# In other words, after the last time a variable is used, we can call `rm()`
# for the variable so that the memory will be freed on the next garbage
# collector run.

library(rstatic)
source("examples.R")

node = quote_ast({
  x = 3
  y = x
  x = x
  y = x
  z = x

  NULL
})

node = to_cfg(node)

# This function finds the final def of every variable (by basename).
find_final_defs = function(node) {
  require("igraph")

  g = node$ssa$graph
  vs = V(g)

  # Skip uses and special vars.
  vs = vs[vs$is_def & !startsWith(vs$name, "._")]

  adjlist = get.adjlist(g, "out")
  basename = vs$basename

  is_final = !vapply(vs, function(v) {
    adj = adjlist[[v]]
    # Check that v has no edges out to vertices with the same basename.
    basename[v] %in% basename[adj]
  }, NA)

  vs[is_final]
}


# ------------------------------------------------------------
# Test 1
#
# This example showed we needed a better way to get info for a variable (by
# basename) in the SSA graph. Now the SSA graph includes a basename attribute
# on each def/use, so this is no longer a problem.

node1 = to_cfg(example1)
final_defs = find_final_defs(node1)

# Now we need to get the uses for each final def. When there's
#
# * 0 uses: Nothing to do.
# * 1 use: Insert `rm()` immediately after.
# * >= 2 uses: This is the tricky case.
#
# For 2 uses, we can use the following algorithm:
#
# 1. Get the block for each use. If they're both in the same block, number the
#    elements of the block and insert `rm()` after the use with the higher
#    number.
# 2. If they're in different blocks, check whether one block dominates the
#    other. If so, insert `rm()` after the use in the dominated block.
# 3. If neither block dominates, insert `rm()` after both uses.
