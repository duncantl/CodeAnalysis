
ifAssignments =
function(node)
{
    node = to_ast(node)
    tr = getAssignedVars(node$true)
    if(!tr$false$is_hidden) {
        fa = getAssignedVars(node$false)
        
    } else {

    }
}

getAssignedVars =
function(node)
{
    node = to_ast(node)
    idx = find_nodes(node, is, "Assign")
    if(length(idx))
        lapply(idx, function(i) node[[i]]$write)
    else
        list()
}

