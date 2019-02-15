#
#

findUsedOptions =
function(code, asNodes = FALSE)
{
    code = to_ast(code)
    idx = find_nodes(code, isUsedOption)
    if(asNodes)
        lapply(idx, function(i) code[[i]])
    else
        lapply(idx, function(i) getUsedOptionName(code[[i]]))
}


isUsedOption =
function(node)
{
   is(node, "Call") && is(node$fn, "Symbol") && ((node$fn$name == "getOption" || (node$fn$name == "options" && length(node$args) > 0)) ) ||  (node$fn$name %in% c("$", "[[", "[") && is(node$args[[1]], "Call") && is(node$args[[1]]$fn, "Symbol") && node$args[[1]]$fn$name == "options")
}


getUsedOptionName =
function(node)
{
    if(node$fn$name %in% c("options", "getOption")) {
        els = node$args
    } else if(node$fn$name == "$") {  # must be  options()$... or [[...]] or [
       return(node$args[[2]]$name)
    } else # [[ or [
        els = node$args[ - 1 ]


    w = sapply(els, is, "Character")
    sapply(els[w], function(x) x$value)    
}
