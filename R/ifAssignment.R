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


ifAssignments =
function(node)    
{
    ifs = findCallsTo(node, "if")
    lapply(ifs, function(x) c(getAssignedVars(x[[3]]),
                              if(length(x) > 3)
                                  getAssignedVars(x[[4]])))
}

getAssignedVars =
    #
    # Find the assignments in a block of code
    # and get the variable that is assignemt
function(node)
{
    a = findAssignsTo(node)
    lapply(a, function(x) x[[2]])
}


if(FALSE) {
    # Do we need this?
    # Determine if two AST nodes are the same eventhough they have different parents.
    setGeneric("equiv", function(x, y, ...)standardGeneric("equiv"))
    setMethod("equiv", c("Symbol", "Symbol"), function(x, y, ...) identical(x$value, y$value))
}
