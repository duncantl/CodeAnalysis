setGeneric("getRHS",
           function(x, asCharacter = TRUE)    
               standardGeneric("getRHS"))


setMethod("getRHS", "ANY", #.default =
          function(x, asCharacter = FALSE)    
              NULL)

setMethod("getRHS", "expression",
function(x, asCharacter = FALSE)
{
    isAssign = sapply(x, class) %in% c("=", "<-")
    ans = sapply(x[isAssign], getRHS, asCharacter = asCharacter)
    names(ans) = sapply(x[isAssign], getLHS, TRUE)
    ans
})


tmp = function(x, asCharacter = FALSE)    
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

setMethod("getRHS", "<-", tmp)
setMethod("getRHS", "=", tmp)



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
