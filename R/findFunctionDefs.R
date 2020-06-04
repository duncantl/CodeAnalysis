findFunctionDefs =
    # Make more robust.
    # [done] put names on the returned list.
    # [done] Add Vectorize
    #
    # f = findFunctionDefs("TOY.R")
    #
function(kode, keepAssignments = TRUE, funsReturningFuns = c("Vectorize") )
{
  if(is.character(kode) && file.exists(kode))
     kode = parse(kode)
  w = sapply(kode, isFunctionDef)

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
