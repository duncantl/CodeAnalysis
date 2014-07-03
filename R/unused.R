findUnusedArgs =
    #
    # Determine if any of the  formal arguments are NOT used in the body of the code.
    # This is a simple test to see if a formal argument is not referenced in the code.
    # It does NOT test to see if it is referenced, but that code could neve be run
    #  e.g.   if(FALSE) {  foo(x)  }
    # 
function(fun, remove = TRUE, clean = TRUE)
{
    if(clean)
        fun = removeConstIf(removeAfterReturn(fun))
    
    sc = new("Script", as.list(body(fun)[-1]))
    inputs = getInputs(sc)
    i = sapply(names(formals(fun)), findWhenUnneeded, info = inputs)
    vars = names(formals(fun))[ is.na(i) ]
    if(remove) {
       paramNames = names(formals(fun))
       i = match(vars, paramNames)
       formals(fun) = formals(fun)[ - i]
       fun
    } else
        vars
}



findUnusedAssignments =
function(fun, remove = TRUE, clean = TRUE)
{
    if(clean)
        fun = removeConstIf(removeAfterReturn(fun))
    
    sc = new("Script", as.list(body(fun)[-1]))
    inputs = getInputs(sc)

    out = lapply(seq(along = inputs),
                 function(i) {
                    rest = inputs[-(1:i)]
                    sapply(inputs[[i]]@outputs, function(var) !isUsed(var, rest))
                 })
# This is probably a little too simplistic.
# It is possible we have a = b = rhs
# and we would assume neither a or b or reused.    

    i = sapply(out, function(x) length(x) && x)
    if(remove) {
        b = body(fun)
        b[which(i) + 1L] = mapply(removeAssign, b[-1][i],  out[i])
        b = b[ ! sapply(b, is.null) ]
        body(fun) = b
        fun
    } else
       unlist(lapply(out[i], names))        
}

isUsed =
function(var, exprInfo)
{
  any(sapply(exprInfo, function(node) var %in% node@inputs))
}

removeAssign =
function(e, vars)
{
  ids = names(vars)
  if(as.character(e[[2]]) %in% ids) {
      sideEffect = hasSideEffect(e[[3]])
      if(!is.na(sideEffect) && !sideEffect)
          NULL
      else
          e[[3]]
  } else {
     e[[3]] = removeAssign(e[[3]], vars)
     e
 }
}

hasSideEffect =
function(expr, ...)
{
  !is.atomic(expr)
}


