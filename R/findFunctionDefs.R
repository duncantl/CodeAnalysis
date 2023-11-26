FunctionsReturningFunctions = "Vectorize"

findFunctionDefs =
    #
    # XXX Probably want getFunctionDefs(), not this one.
    # getFunctionDefs() is generic and handles all sorts of different inputs
    #  and is recursive so finds functions in functions.
    #
    # !!!   This does understand calls that return functions, e.g., Vectorize(f)
    #
    # Find top-level function definitions of the form
    #   g = function()...
    # Also handles
    #   g = Vectorize(f)
    #
    # and the caller can specify names of functions which return functions.
    #
    # Make more robust.
    # Handle if(FALSE) and if(TRUE). if(FALSE) will just work because we are only looking for assignments.
    # [done] put names on the returned list.
    # [done] Add Vectorize
    #
    # f = findFunctionDefs("TOY.R")
    #
function(kode, keepAssignments = FALSE, funsReturningFuns = FunctionsReturningFunctions, eval = FALSE)
{
  if(is.character(kode) && file.exists(kode))
      kode = parse(kode)
  
  w = sapply(kode, isFunctionDef, funsReturningFuns)

  if(length(w) == 0 || all(!w))
      return(list())
  
  ans = kode[w]
  names(ans) = sapply(ans, function(x) if(is.name(x[[2]])) as.character(x[[2]]) else "")

  if(!keepAssignments)
      ans = lapply(ans, `[[`, 3)

  if(isTRUE(eval))
      eval = globalenv()

  if(is.environment(eval))
      ans = lapply(ans, base::eval, eval)
  
  ans
}


isFunctionDef =
    # An assignment of a function.
function(x, funsReturningFuns = FunctionsReturningFunctions)
{
    is.call(x) && length(as.character(x[[1]])) == 1 && as.character(x[[1]]) %in% c("=", "<-") && is.call(x[[3]]) &&
        (length(as.character(x[[3]][[1]])) == 1 && (as.character(x[[3]][[1]]) == "function" || as.character(x[[3]][[1]]) %in% funsReturningFuns))
}

findIndirectFunctions =
    # need better name
    # Looks for calls to functions named in funsReturningFuns
function(code, funsReturningFuns = FunctionsReturningFunctions)
{
  if(is.character(code) && file.exists(code))
      code = parse(code)
  
  w = sapply(code, isIndirectFunctionDef)
}

isIndirectFunctionDef =
function(x, funsReturningFuns = FunctionsReturningFunctions)
{
    is.call(x) && as.character(x[[1]]) %in% c("=", "<-") && is.call(x[[3]]) &&
         as.character(x[[3]][[1]]) %in% funsReturningFuns
}

getArgFromCall =
    #
    #   getArgFromCall( quote(Vectorize(f)),  FUN)
    #  or "FUN"
    #
function(call, arg, asCharacter = TRUE)
{
    id1 = substitute(arg)
    if(is.numeric(id1))
        ans = call[[arg + 1]]
    else {
        id = as.character(id1)
        m = match(id, names(call))
        if(is.na(m)) {
            def = get(as.character(call[[1]]), mode = "function")
            call2 = match.call(def, call)
            m = match(id, names(call2))
            if(is.na(m))
                stop("no named argument ", id, " in call ", paste(deparse(call), collapse = " "))
            ans = call2[[m]]
        } else
            ans = call[[m]]

    }
    if(asCharacter)
        deparse(ans)
    else
        ans
}



#inline

getIfCond = function(code) code[[2]]
getIfTrue = function(code) code[[3]]
getIfFalse = function(code) if(length(code) > 3) code[[4]] else NULL


cleanConstantIfs =
    #
    # This should use constant propogation.
    #
function(code)
{
      if(is.character(code) && file.exists(code))
          code = parse(code)

      isIf = sapply(code, is, "if")
      if(!any(isIf))
          return(code)

#      ifs = code[isIf]
#      conds = lapply(ifs, getIfCond)

      code[isIf] = lapply(code[isIf],
                         function(x) {
                             cond = getIfCond(x)
                             if(isFALSE(cond))
                                 NULL
                             else
                                 x
                         })

      code[!sapply(code, is.null)]
}
