

findNSE =
function(code, asNodes = TRUE)
{
    code = to_ast(code)
    find_nodes(code, isNSE)
#  if(length(idx))
#      lapply(idx, function(i) code[[i]])
#  else
#      list()
}

NSEFunNames = c("eval", "evalq", "get", "assign", "rm", "eval.parent", "exists")

isNSE =
function(node, nseFunNames = NSEFunNames)
{
    is(node, "Call") && is(node$fn, "Symbol") && node$fn$value %in% nseFunNames
}
