constInputs =
function(f)
{
    sc = new("Script", as.list(body(f)[-1]))
    inputs = getInputs(sc)
    outs = sapply(inputs, slot, "updates")
    i = match(names(formals(f)), unlist(outs))
    names(formals(f))[is.na(i)]
}
