#
#

findUsedOptions =
function(code)
{
    code = to_ast(code)
    idx = find_nodes(code, isUsedOption)
    lapply(idx, function(i) code[[i]])
}


isUsedOption =
function(node)
{
   is(node, "Call") && is(node$fn, "Symbol") && ((node$fn$name == "getOption" || (node$fn$name == "options" && length(node$args) > 0)) ) ||  (node$fn$name %in% c("$", "[[") && is(node$args[[1]], "Call") && is(node$args[[1]]$fn, "Symbol") && node$args[[1]]$fn$name == "options")
}
