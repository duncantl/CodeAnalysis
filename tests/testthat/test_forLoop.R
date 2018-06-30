test_that("for loop to lapply", {

    loop1 = quote(for(i in x){f(i)})
    actual = forLoopToLapply(loop1)
    expected = quote(lapply(x, function(i){f(i)}))

    expect_equal(actual, expected)

    # Can't be parallelized
    loop2 = quote(for(i in x){
        y = f(y)
    })

    expect_equal(forLoopToLapply(loop2), loop2)

    loop3 = quote(for(i in x){
        tmp = foo()
        f(tmp, i)
    })
    actual = forLoopToLapply(loop3)
    expected = quote(lapply(x, function(i){
        tmp = foo()
        f(tmp, i)
    }))

    expect_equal(actual, expected)

})


# For testing interactively
#forLoopToLapply = autoparallel:::forLoopToLapply
#debug(autoparallel:::forloop_with_updates)

test_that("assignment inside for loop", {

    loop1 = quote(
    for (i in 1:500){
        tmp = g() 
        output[[i]] = tmp
    })

    expected = quote(
    output[1:500] <- lapply(1:500, function(i) {
        tmp = g() 
        tmp
    }))

    actual = forLoopToLapply(loop1)

    expect_equal(actual, expected)

    # True dependence, can't parallelize
    loop2 = quote(
    for (i in 1:500){
        tmp = g(tmp) 
        x[[i]] = tmp
    })

    actual = forLoopToLapply(loop2)

    expect_equal(actual, loop2)

    # True dependence, can't parallelize
    loop3 = quote(
    for (i in 2:500){
        x[[i]] = g(x[[i - 1]])
    })

    actual = forLoopToLapply(loop3)

    expect_equal(actual, loop3)

})


if(FALSE){
    # Here's the actual use case I want to capture. 
    # This might be called the "vector growing anti-idiom"
    # g() is actually a block of about 20 lines inside the loop, but
    # since it doesn't depend on i it may as well be a function.

    loop = parse(text = "
    output = list()
    for (i in 1:500){
        tmp = g() 
        output[[i]] = tmp
    }")

    # Here's how I would write it by hand:
    idiomatic = parse(text = "
    output = replicate(500, g(), simplify = FALSE)
    ")

    # Then to convert this to parallel:
    # output = parallel::mclapply(1:500, function(...) g())

    # But I think it would be easier to automatically transform it into an
    # lapply, something like:
    lapply1 = parse(text = "
    output = lapply(1:500, function(i) {
        tmp = g() 
        tmp
    }")

    # If output was pre allocated, as it should be in this case, then it
    # would be more difficult for me to know how to rewrite the loop. I
    # could do something like this:
    lapply2 = parse(text = "
    .tmp_iterator = 1:500
    output[.tmp_iterator] = lapply(.tmp_iterator, function(i) {
        tmp = g() 
        tmp
    }
    rm(.tmp_iterator)
    ")

    # Probably no big deal to repeat the iterator code twice, especially
    # with ALTREP.
    lapply3 = parse(text = "
    output[1:500] = lapply(1:500, function(i) {
        tmp = g() 
        tmp
    }
    ")

    # Approaches 2 and 3 have the advantage of allowing for the iterator to
    # NOT be a simple sequence, and they should still match the result of
    # the for loop. I think I'll go with this way.

    # Unless we can reliably differentiate vectors and lists based on
    # static analysis I think we should just default to a list. The use of
    # `[[` usually means it's a list, even though it works with vectors.

    # Just thought of another perversion. What if the user redefines the
    # iterator variable inside the loop?
    # for(i in 1:n){
    #   i = ...
    # Ack!
}
