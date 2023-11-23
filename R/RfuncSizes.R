# This file came from ~/Book/HackingREngine/Bugs/trace/RfuncSizes.R
# with the purpose of computing a data.frame of the number of calls
# in each function in each R package.
#

numCalls =
function(code)
{
    length(findCallsTo(code))
}

getPkgFunctions =
    # Does getFunctionDefinitions() do this already.
function(pkg)
{
    if(!require(pkg, character.only = TRUE))
        return(list())
    objs = as.list.environment(getNamespace(pkg), TRUE)
    objs[ sapply(objs, is.function) ]
}


