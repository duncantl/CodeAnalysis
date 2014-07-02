findUnusedArgs =
function(f)
{
    sc = new("Script", as.list(body(f)[-1]))
    inputs = getInputs(sc)
    i = sapply(names(formals(f)), findWhenUnneeded, info = inputs)
    names(formals(f))[ is.na(i) ]
}

