paramsAfterDots =
function(f, params = names(formals(f)))
{
    i = match("...", params)
    if(is.na(i) || i == length(params))
        return(character())

    params[seq(i+1L, length(params))]
}


# Add a function that analyzes calls to functions
# and checks if that function definition has ... and if
# it does
