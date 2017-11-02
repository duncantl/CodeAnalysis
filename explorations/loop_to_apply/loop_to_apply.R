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
    if( replacer$fn$basename != "[<-" ) return(NULL)

    # test RHS subset
    ss = replacer$args[[3]]$args[[1]]
    if( !is(ss, "Subset") ) return(NULL)

    # index, ivar, and subset function index should all be the same
    if( replacer$args[[2]]$basename != forloop$ivar$basename ||
        forloop$ivar$basename != ss$args[[2]]$basename ) return(NULL)

    # Passes checks, convert to sapply

    # Clark: The following is the clearest and easiest way for me to write
    # this code. Is there any advantage to using the constructors from
    # rstatic?

    map = list(fun = replacer$args[[3]]$fn$basename
        , lhs = replacer$args[[1]]$basename
        , arg = ss$args[[1]]$basename
        )

    map = lapply(map, as.symbol)

    substitute(lhs <- sapply(fun, arg), env = map)
}


# Testing
############################################################

code = parse("loops.R")

ex1 = to_ast(code[1:2])

loop_to_sapply(ex1$body[[2]])


ex1b = to_ast(code[[5]])

# TODO: fix below use case:
loop_to_sapply(ex1b)
