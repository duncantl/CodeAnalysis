findUnusedArgs =
    #
    # Determine if any of the  formal arguments are NOT used in the body of the code.
    # This is a simple test to see if a formal argument is not referenced in the code.
    # It does NOT test to see if it is referenced, but that code could neve be run
    #  e.g.   if(FALSE) {  foo(x)  }
    # 
function(fun)
{
    sc = new("Script", as.list(body(fun)[-1]))
    inputs = getInputs(sc)
    i = sapply(names(formals(fun)), findWhenUnneeded, info = inputs)
    names(formals(fun))[ is.na(i) ]
}



findUnusedAssignments =
function(fun)
{
    sc = new("Script", as.list(body(fun)[-1]))
    inputs = getInputs(sc)

    out = lapply(seq(along = inputs),
                 function(i) {
                    rest = inputs[-(1:i)]
                    sapply(inputs[[i]]@outputs, function(var) !isUsed(var, rest))
                 })
    i = sapply(out, function(x) length(x) && x)
    unlist(lapply(out[i], names))
}

isUsed =
function(var, exprInfo)
{
  any(sapply(exprInfo, function(node) var %in% node@inputs))
}
