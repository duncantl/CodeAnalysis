library(rstatic)


#" Convert For Loop To Sapply
#" 
#" Tests for a very specific pattern inside the for loop, see example 1.
#" 
#" @param forloop rstatic::ASTNode representing a for loop
#" @value sapply language object
loop_to_sapply = function(forloop)
{
    # TODO: Check if { is used.
    replacer = forloop$body$body[[1]]$read

    # If we continue to develop this then we'll want to test and reject for
    # more conditions. Rather than having many nested if() statements we
    # can just return NULL when assumptions fail.

    # test index assignment
    if( replacer$fn$basename != "[<-" ) return(forloop)

    # test RHS subset
    ss = replacer$args[[3]]$args[[1]]
    if( !is(ss, "Subset") ) return(forloop)

    # index, ivar, and subset function index should all be the same
    if( replacer$args[[2]]$basename != forloop$ivar$basename ||
        forloop$ivar$basename != ss$args[[2]]$basename ) return(forloop)

    # Passes checks, convert to sapply

    map = list(fun = replacer$args[[3]]$fn$basename
        , lhs = replacer$args[[1]]$basename
        , arg = ss$args[[1]]$basename
        )

    map = lapply(map, as.symbol)

    substitute(lhs <- sapply(fun, arg), env = map)
}


