
#
# The idea here is to find functions which take a parameter
# and then call/invoke that parameter as a function, e.g. lapply, mapply, curve, optim, do.call, outer
# RCIndex::parseTU() and visitTU() (although that hits C code)
#
# I think I implemented this. Look in the R files.
#  findCallsParam ???
#
if(FALSE) {

  foo = function(x, g, f)    
           lapply(split(x, g), f)

  foo1 = function(files, f) {
             tmp = lapply(files, f)
             do.call(rbind, tmp)
         }
}


findCallsParam =
    #
    # Find any direct calls to a parameter, e.g.
    #   optim() may call its fn and gr
    #
    # Doesn't find indirect calls such as function(x, f) sapply(x, f).
    # Get those with getGlobals()
    #
    # Does't detect calls from C code of course. See the RCIndex package 
    # and NativeCodeAnalysis package for that..
    #
function(fun)
{
   params = names(formals(fun))
   funa = to_ast(fun)
   v = find_nodes(funa, function(x) is(x, "Call") && is(x$fn, "Symbol") && x$fn$value %in% params)
   vapply(v, function(x) x$fn$value, character(1))
}

