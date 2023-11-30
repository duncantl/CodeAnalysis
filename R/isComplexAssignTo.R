if(FALSE) {
    # eval(parse("isComplexAssignTo.R")[[1]][[3]])
    es = list(e1 = quote(foo(x) <- a),
              e2 = quote(x$y <- a),
              e3 = quote(x[[1]][[2]][[3]] <- a),
              e4 = quote(foo(x) <- a),
              e5 = quote(foo(x, y) <- a),
              e6 = quote(foo(x)[[1]][[2]] <- a),
              e7 = quote(names(x)[[2]][[3]] <- a)
              )

    tmp = lapply(es, function(e) {
                       c(x = isComplexAssignTo(e, "x"),
                         y = isComplexAssignTo(e, "y"),
                         a = isComplexAssignTo(e, "a"))
    })
    
    stopifnot( all ( sapply(tmp, identical, c(x = TRUE, y = FALSE, a = FALSE)) ))

    stopifnot(!isComplexAssignTo(quote(foo(g(x), a)[[1]][[2]] <- a), "x"))
    stopifnot(!isComplexAssignTo(quote(foo(g(x), a)[[1]][[2]] <- a), "a"))
    stopifnot(!isComplexAssignTo(quote(foo(g(x), a)[[1]][[2]] <- a), "foo"))

    all(sapply(es, function(x) as.character(getReplacementTargetVariable(x[[2]]))) == "x")
}


if(FALSE) {

    # Example of a replacement that has nested calls identifying the assignment variable
    # and then goes wrong. R does not catch this and assigns.
    #
    `foo<-` = function(x, a = 1, ..., value) {  x[[length(x) + 1]] = paste("new", value, a, sep = ".") ; x}
    x = list()
    foo(x) = 20
    foo(x) = 30
    foo(x, "ab") = 30
    x
    g = function(a) seq_len(length(a))
    g(x)

    # dput(x)
    # list("new.20.1", "new.30.1", "new.30.ab")
    
    foo(g(x), "???") = 40
    # error because  needs g<-
    # what if we define g<-
    `g<-` = function(x, ..., value) value

    foo(g(x), "???") = 40    
    # Now x is a character
    # c("1", "2", "3", "new.40.???")


    # What about this odd one?
    # quote((for(i in x) x ) <- 1)
    # parse works
    # evaluating this gets an error
    #  Error in (for (i in x) x) <- 1 : object 'i' not found
    # So trying to assign to i.
    # Can't trace for<- since doesn't exist.  Could define to trace.
}



isComplexAssignTo =
    #
    #
    # foo(x) <- 1  -> e1[[2]] is foo(x) and e[[1]] is <-
    # x$y <- 1  ->  e2[[2]] is x$y and e[[1]] is <-
    # e3 ->  e2[[2]]   x[[1]][[2]][[3]] and e[[1]] is <-
    #     need to follow the sequence of subsetting to get to x[[1]] and get [[2]] of that
    #
    # Wrong: findAssignsTo2(quote(x[[1]][[2]][[3]] <- 4), "x")
    #
function(x, var, simpleOk = TRUE)    
{
    if(simpleOk && isSimpleAssignTo(x, var))
        return(TRUE)

    if(! isCallTo(x, c("<-", "=")))  # "$<-", "[[<-", "[<-",
        return(FALSE)
    
    y = x[[2]]
    # Now be general enough to handle the cases above.
    # Basically find any reference to any of the elements of var - as long as it is not a local variable.
    # see if var is in any of these symbols

    # Too simple - could be foo(a, x) and looking for x. Is this a match?
    # any(all_symbols(y) %in% var)
    tvar = getReplacementTargetVariable(y)

    if(length(tvar))
        as.character(tvar) %in% var
    else
        FALSE
}

getReplacementTargetVariable =
function(x)
{
    if(is.name(x))
        return(x)

    if(is.call(x)) {
        v = x[[1]]
        if(isSymbol(v, c("$", "[[", "[")))
            # keep walking down the sequence
            # x[[1]][[2]][[3]] = v
            return(getReplacementTargetVariable(x[[2]]))        

        if(is.name(x[[2]]))
            return(x[[2]])

# message("call but first element is not a call to usual suspects and first argument is not a symbol: ", deparse(x[[2]]))        
    }

    character()
}


