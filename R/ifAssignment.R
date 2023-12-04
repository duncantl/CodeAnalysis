#
# Find assignments in the bodies of if() and else
# which assign to the same variable and which could
# be 
#
# Want to catch  a$b = ... as well as x = 
#
# This currently works on an if() statement
# Need to generalize so works on other AST objects or R language objects
# e.g. an Assign, etc.
#
#
ifAssignments =
function(node)
{
    node = to_ast(node)

    # Get the assignments from the body of the if() part (the true part)
    tr = getAssignedVars(node$true)

    # if there is an else, get the assignments in there
    if(length(node$false$contents)) {
        fa = getAssignedVars(node$false)
        if(length(fa) > 0 && length(tr) > 0)
            equiv(fa[[length(fa)]], tr[[length(tr)]])
        else
            FALSE
    } else {
        FALSE
    }

}

getAssignedVars =
    #
    # Find the assignments in a block of code
    # and get the variable that is assignemt
function(node)
{
    node = to_ast(node)
    idx = find_nodes(node, is, "Assign")
    if(length(idx))
        lapply(idx, function(i) i$write)
    else
        list()
}


# Do we need this?
# Determine if two AST nodes are the same eventhough they have different parents.
setGeneric("equiv", function(x, y, ...)standardGeneric("equiv"))

#??? Is this in rstatic?
setMethod("equiv", c("Symbol", "Symbol"), function(x, y, ...) identical(x$value, y$value))

