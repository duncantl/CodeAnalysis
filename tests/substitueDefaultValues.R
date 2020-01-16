f = function(a, b = 4, w = "hi") {
    print(w)
    a+b
}

fcorrect = function (a, b, w, .missingCall) 
{
    if (.missingCall["w"]) 
        w <- "hi"
    print(w)
    if (.missingCall["b"]) 
        b <- 4
    a + b
}

fval = CodeAnalysis:::substituteDefaultValues(f)

comp = all.equal(fval, fcorrect, check.attributes=FALSE)

stopifnot(is.logical(comp) && comp)
