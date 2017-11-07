# This script has examples and code of using CodeDepends to insert `rm()` after
# a variable stops being live.

library(CodeDepends)
source("examples.R")

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

# ------------------------------------------------------------
# Test 1
# NOTE: Does `getInputs()` not work with a user-defined function as the
# argument? This seems to be the only way to make it work:
sc = new("Script", as.list(body(example1)[-1]))

info = getInputs(sc)

last_uses = get_last_uses(sc, info)
result1 = insert_rm(sc, last_uses)

# ------------------------------------------------------------
# Test 2
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
