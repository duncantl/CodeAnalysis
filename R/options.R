#
#

findUsedOptions =
function(code, asNodes = FALSE)
{
    code = to_ast(code)
    idx = find_nodes(code, isUsedOption)
    if(asNodes)
        idx
    else
        sapply(idx, getUsedOptionName)
}


isUsedOption =
function(node)
{
    is(node, "Call") && is(node$fn, "Symbol") && ((node$fn$value == "getOption" || (node$fn$value == "options" && length(node$args) > 0)) ) ||
        (node$fn$value %in% c("$", "[[", "[") && is(node$args[[1]], "Call") && is(node$args[[1]]$fn, "Symbol") && node$args[[1]]$fn$value == "options")
}


getUsedOptionName =
function(node)
{
    if(node$fn$value %in% c("options", "getOption")) {
        els = node$args$contents
    } else if(node$fn$name == "$") {  # must be  options()$... or [[...]] or [
       return(node$args[[2]]$name)
    } else # [[ or [
        els = node$args[ - 1 ]


    w = sapply(els, is, "Character")
    sapply(els[w], function(x) x$value)    
}
