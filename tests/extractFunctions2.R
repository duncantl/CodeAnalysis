#
# Should we find the chain in isNamedFunctionAssign
# The actual case in tools is very special. It is copying the
# function and then changing the formals() on each to something different.
# So we wouldn't want to extract them.
#
# We would want to do better code analysis and find the uses of each symbol
# and see are they being called or updated.
#
#
#

library(CodeAnalysis)

g =
function()
{
    a = 1
    f1 = f2 = function(x) x + 1
    b = 2
    f1(a) + f2(b)
}
