
isRemoveAllVars =
    #
    #` @title Determines which expressions are of the form rm(list = ls())
    #
function(e, remove = FALSE, asIndex = TRUE)
{    
    w = sapply(e, function(x) is.call(x) &&  as.character(x[[1]]) %in% c("remove", "rm"))
    if(!any(w)) 
        return(if(remove) e else if(asIndex) integer() else list())

    w2 = sapply(e[w], isRemoveAllCall)

    i = which(w)[w2]
    if(remove)
        e[-i]
    else if(asIndex)
           i
         else e[i]
}


isRemoveAllCall =
function(x)
{
    # Currently checks only for list = ls(...)) where we could have anything in ...
   !is.na(i <- match("list", names(x))) &&
   is.call(x[[i]]) &&
   as.character(x[[i]][[1]]) %in% c("ls", "objects") &&
      # Check if using a different environment    
   (is.na( i <-  match("envir", names(x))) || (is.call(x[[i]]) && as.character(x[[i]]) == "globalenv"))
}
