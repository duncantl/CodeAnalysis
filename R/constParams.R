constInputs =
    #
    #
    #  Need to process recursively to see if functions
    #  this function calls and passes arguments modify those or not.
    #
function(f)
{
    sc = new("Script", as.list(body(f)[-1]))
    inputs = getInputs(sc)
    outs = sapply(inputs, slot, "updates")
    i = match(names(formals(f)), unlist(outs))
    names(formals(f))[is.na(i)]
}
