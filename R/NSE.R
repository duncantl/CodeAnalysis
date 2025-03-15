
NSEFunNames = c("eval", "evalq", "get", "assign", "rm", "eval.parent", "exists")

findNSE =
function(code, asNodes = TRUE, nseFunNames = NSEFunNames)
{
    findCallsTo(code, nseFunNames)
}



