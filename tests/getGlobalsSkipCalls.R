# what about calls to until with calls to substitute
# need a predicate function on the call object.

library(CodeAnalysis)

foo =
function(x)
{
    until(x, quote( length(driver$findElements(value = "//foo")) > 0))
}

bar =
function(x)
{
    cur = 1
    until(x, substitute( as.integer(xpathSapply(doc, "//bar", xmlValue)) > num, list(num = cur)))
}

bar2 =
function(x)
{
    cur = 1
    until(other, substitute( as.integer(xpathSapply(doc, "//bar", xmlValue)) > num, list(num = cur)))
}

bar3 =
function(x, y)    
{
    x == Back
}

bar4 =
function(x)    
{
    x == pkg::abc
}

a = getGlobals(foo)
stopifnot(a$variables == "driver")

b = getGlobals(bar, skipCallsTo = "until")
stopifnot(length(b$variables) == 0)

c = getGlobals(bar2, skipCallsTo = "substitute")
stopifnot(c$variables == "other")

c = getGlobals(bar2, skipCallsTo = "substitute", availableVars = c("other"))
stopifnot(length(c$variables) == 0)

d = getGlobals(bar3, availableVars = c("Back"))
stopifnot(length(d$variables) == 0)




