findFunctionDefs =
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
function(kode, keepAssignments = FALSE, funsReturningFuns = c("Vectorize") )
{
  if(is.character(kode) && file.exists(kode))
     kode = parse(kode)
  w = sapply(kode, isFunctionDef)

  if(length(w) == 0)
      return(list())

  ans = kode[w]
  names(ans) = sapply(ans, function(x) if(is.name(x[[2]])) as.character(x[[2]]) else "")

  if(!keepAssignments)
      ans = lapply(ans, `[[`, 3)
      
  ans
}


isFunctionDef =
function(x, funsReturningFuns = c("Vectorize")) 
{
    is.call(x) && as.character(x[[1]]) %in% c("=", "<-") && is.call(x[[3]]) && (as.character(x[[3]][[1]]) == "function" || as.character(x[[3]][[1]]) %in% funsReturningFuns)
}

findIndirectFunctions =
# need better name    
function(code, funsReturningFuns = c("Vectorize")) 
{
  if(is.character(code) && file.exists(code))
     code = parse(code)
  w = sapply(kode, isIndirectFunctionDef)
}

isIndirectFunctionDef =
function(x, funsReturningFuns = c("Vectorize")) 
{
    is.call(x) && as.character(x[[1]]) %in% c("=", "<-") && is.call(x[[3]]) && as.character(x[[3]][[1]]) %in% funsReturningFuns
}


FunctionsReturningFunctions = "Vectorize"

isCallTo =
    #
    #
    #
function(x, funs)
{
    is.call(x) && is.name(x[[1]]) && as.character(x[[1]]) %in% funs
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

getRHS =
function(x, asCharacter = TRUE)    
{
    UseMethod("getRHS")
}

getRHS.default =
function(x, asCharacter = FALSE)    
    NULL

getRHS.expression =
function(x, asCharacter = FALSE)
{
    isAssign = sapply(x, class) %in% c("=", "<-")
    ans = sapply(x[isAssign], getRHS, asCharacter = asCharacter)
    names(ans) = sapply(x[isAssign], getLHS, TRUE)
    ans
}


`getRHS.<-` = `getRHS.=` =
function(x, asCharacter = FALSE)    
{
    # Check is an assignment
    # 1 -> x is of class <-, so parser already converted it.
    if(!(class(x) %in% c("=", "->")))
        return(NULL)
    
    ans = x[[3]]
    if(asCharacter)
        structure(deparse(ans), names = deparse(x[[2]])) #XXX handle calls on lhs.
    else
        ans
}


getLHS =
function(x, asCharacter = FALSE, simpleVar = TRUE)
    UseMethod("getLHS")

getLHS.default =
function(x, asCharacter = FALSE, simpleVar = TRUE)    
    NULL

getLHS.expression =
function(x, asCharacter = FALSE, simpleVar = TRUE)
{
    isAssign = sapply(x, class) %in% c("=", "<-")
    tmp = x[isAssign]

    if(simpleVar)
        tmp = tmp[ sapply(tmp, function(x) is.name(x[[2]])) ]
    
    tmp = lapply(tmp, `[[`, 2)

    if(asCharacter)
        tmp = sapply(tmp, as.character)

    tmp
}




#inline

getIfCond = function(code) code[[2]]
getIfTrue = function(code) code[[3]]
getIfFalse = function(code) if(length(code) > 3) code[[4]] else NULL


cleanConstantIfs =
    #
    #
    # This should use constant propogation.
    #
function(code)
{
      if(is.character(code) && file.exists(code))
          code = parse(code)

      isIf = which(sapply(code, is, "if"))
      if(length(isIf) == 0)
          return(code)

      ifs = code[isIf]
      conds = lapply(ifs, getIfCond)
      
}
