MatchCall =
function(call, def, expand.dots = TRUE)
{
    paramNames = pnames = names(formals(def))

    dotsPos = match("...", paramNames)
#browser()    
    args = call$args$contents
    origArgNames = argNames = names(args)
    m = match(argNames, pnames, 0)
    # if we have matches, take those out of pnames
    if(any(m != 0))
        pnames = pnames[-m]

    if(!is.na(dotsPos) && dotsPos <= length(pnames))
        pnames = pnames[ -c( dotsPos:length(pnames)) ]
    
    # match partial names, but not after ...
    m2 = pmatch(argNames[m == 0], pnames)

    if(any(!is.na(m2))) {
        # complete the argument names
        argNames[which(m==0)[!is.na(m2)]] = pnames[m2[!is.na(m2)]]
        pnames = pnames[ - m2[!is.na(m2)] ]
    }

        # Now match by position.
    w = (argNames == "")
    argNames[w] = pnames[seq_len(sum(w))]
    names(args) = argNames

    browser()
    w2 = !(argNames %in% paramNames)
    if(any(w2)) {
        if(is.na(dotsPos))
            stop("extra arguments: ", paste(argNames[w2], collapse = ", "))
        if(!expand.dots) {
            tmp = args[w2]
            args = args[!w2]
            args$"..." = tmp
        }
    }
    
    # change the order to put them in the order of the parameters/formals in the function definition
    m = match(paramNames, argNames, 0)
    nargs = c(args[m], args[m == 0])
    na = is.na(names(nargs))
    names(nargs)[na] = ""
    
    empty = sapply(nargs, is, "EmptyArgument")
    call$args$contents = nargs[!empty]
    
    call
}

if(FALSE) {
    a = to_ast(quote(structure(y, class = NULL, row.names = NULL)))

    b = to_ast(quote(structure(class = NULL, row.names = NULL, y)))
    c = to_ast(quote(lm(y ~ x, mtcars, model = TRUE, qr = FALSE)))
    MatchCall(c, lm)
    
    d = to_ast(quote(lm(y ~ x, d = mtcars, model = TRUE, qr = FALSE)))    
    d1 = MatchCall(d, lm)
    identical(d, d1) # same object/instance

    identical(c, d) # NO
    c == d
    
    e = to_ast(quote(lm(d = mtcars, y ~ x, model = TRUE, qr = FALSE)))
    d1 == e


    foo = function(x, y = 2, ..., .na.rm = TRUE){}
    f = quote(foo(, 3, .na.r = FALSE,  other = 1))
    fa = to_ast(f)
    MatchCall(fa, foo)
    match.call(foo, f, expand.dots = TRUE)
    match.call(foo, f, expand.dots = FALSE)    


    Sum = function (..., na.rm = FALSE)  .Primitive("sum")
    g = quote(Sum(x, y = 1:10, z, .na.rm = TRUE))
    match.call(Sum, g)
    MatchCall(to_ast(g), Sum)
}
