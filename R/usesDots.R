
usesDots =
    # In a function body (node), how does it use ...
    #
function(node)
{
    node = to_ast(node)
    #XXX need to work on the default values for the parameters also
    c( unlist(lapply(node$params, usesDotsI), recursive = FALSE), usesDotsI(node$body))
}

usesDotsI =
function(node)
{            
    idx = find_nodes(node, isDots)
    if(length(idx))
        lapply(idx, function(i) i$parent)
    else
        list()
}

isDots =
function(node)
{
  is(node, "Symbol") && node$name == "..."
}
