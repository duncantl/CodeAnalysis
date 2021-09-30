library(rstatic)

match_call = MatchCall =
function(call, definition, expand.dots = TRUE)
{

    if(typeof(definition) != "closure")
       stop("invalid 'definition' argument: not a closure but a ", typeof(definition))
    
    paramNames = pnames = names(formals(definition))

    dotsPos = match("...", paramNames)

    args = call$args$contents
    if(is.null(names(args)))
       names(args) = rep("", length(args))
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

#    browser()

    # change the order to put them in the order of the parameters/formals in the function definition
    m = match(paramNames, argNames, 0)
    nargs = c(args[m], args[!(argNames %in% paramNames)])
    na = is.na(names(nargs))
    names(nargs)[na] = ""
    argNames = names(nargs)
    
    w2 = !(argNames %in% paramNames)
    if(any(w2)) {
        if(is.na(dotsPos))
            stop("unused argument(s): ", paste(sapply(args[w2],function(x) deparse(as_language(x))), collapse = ", "), sprintf(" in position(s) %s", paste(which(w2), collapse =", ")))
        
        if(!expand.dots) {
            tmp = args[w2]
            args = args[!w2]
            args$"..." = tmp
        }
    }
    

    # Drop any empty argumens introduced be, e.g, foo(, 2)
    empty = sapply(nargs, is, "EmptyArgument")
    call$args$contents = nargs[!empty]
    
    call
}

if(FALSE) {
    a = quote(structure(y, class = NULL, row.names = NULL))
    a2 = MatchCall(to_ast(a), structure)  #XXX where did row.names go!
    stopifnot(identical(as_language(a2), match.call(structure, a)))
    
    b = quote(structure(class = NULL, row.names = NULL, y))
    b2 = MatchCall(to_ast(b), structure) #XXX class goes missing!
    stopifnot(identical(as_language(b2), match.call(structure, b)))
    
    c = quote(lm(y ~ x, mtcars, model = TRUE, qr = FALSE))
    c2 = MatchCall(to_ast(c), lm)
    stopifnot(identical(as_language(c2), match.call(lm, c)))
    
    
    d = to_ast(quote(lm(y ~ x, d = mtcars, model = TRUE, qr = FALSE)))    
    d2 = MatchCall(d, lm)
    stopifnot(identical(d, d2)) # same object/instance

    stopifnot(!identical(c, d)) # NO
    stopifnot(c2 == d2)
    
    e = quote(lm(d = mtcars, y ~ x, model = TRUE, qr = FALSE))
    stopifnot(d2 == MatchCall(to_ast(e), lm))


    foo = function(x, y = 2, ..., .na.rm = TRUE){}
    f = quote(foo(, 3, .na.r = FALSE,  other = 1))
    f2 = MatchCall(to_ast(f), foo)
    stopifnot(identical(as_language(f2), match.call(foo, f, expand.dots = TRUE)))
    match.call(foo, f, expand.dots = FALSE)    #XXX ... = pairlist(.na.r = FALSE,  other = 1)


    Sum = function (..., na.rm = FALSE)  .Primitive("sum")
    g = quote(Sum(x, y = 1:10, z, .na.rm = TRUE))
    g2 = MatchCall(to_ast(g), Sum)
    stopifnot(identical(match.call(Sum, g), as_language(g2)))

    # unused arguments
    tryCatch(match.call(function(x){}, quote(bar(1, 120))))
    tryCatch(MatchCall(to_ast(quote(bar(1, 120))), function(x){}))
}

if(FALSE) {
    e = parse("matchCall.R")
    env = new.env()
    lapply(as.list(e[[length(e)-1]][[3]]), eval, env)
}
