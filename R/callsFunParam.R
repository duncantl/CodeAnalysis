
# I think I already implemented this. Look in the R files.
#  findCallsParam ???
# # !!! Look in freeVariables.R  - usedAsFunction and isParamUsedAsFun
#
# The idea here is to find functions which take a parameter
# and then call/invoke that parameter as a function, e.g. lapply, mapply, curve, optim, do.call, outer
# RCIndex::parseTU() and visitTU() (although that hits C code). Or Rllvm.
#
#
#
#
if(FALSE) {

  foo = function(x, g, f)    
           lapply(split(x, g), f)

  foo1 = function(files, f) {
             tmp = lapply(files, f)
             do.call(rbind, tmp)
  }

  foo3 = function(files, f, g) {
             tmp = lapply(files, f)
             do.call(g, tmp)
         }    


  foo2 = function(x,f)    
           f(x + 1) + 2
}


findCallsParam =
    #
    # Find any direct calls to a parameter, e.g.
    #   optim() may call its fn and gr
    #
    # ? Doesn't find indirect calls such as function(x, f) sapply(x, f).
    # Get those with getGlobals().
    # Actually, findCallsTo does find use in lapply()
    #
    # Doesn't detect calls from C code of course. See the RCIndex package 
    # and NativeCodeAnalysis package for that..
    #
function(fun, asCalls = FALSE)
{
    params = names(formals(fun))

    v = findCallsTo(fun, params)

    if(asCalls)
        v
    else
          # should we use deparse rather than as.character(). Shouldn't matter as direct call to parameter name.
       unique(vapply(v, function(x) as.character( x[[1]] ), character(1)))
}

