test_that("replacing functions", {

    expr = parse(text = "
        # Testing code:
        n = 1000000L
        p = 20L
        x = matrix(1:(n*p), ncol = p)
        x
        colmaxs = apply(x, 2, max)
        colmaxs2 <- apply(x, 2, max)
        assign('colmaxs3', apply(x, 2, max))
        apply(x, 2, min)
    ")

    sub_one_docall(expr, list(apply = quote(FANCY_APPLY)))

})


test_that("find_var", {

    expr = quote(x[1:5])
    expect_equal(find_var(expr, "x"), list(2))

    expr = quote(y[1:5])
    expect_equal(find_var(expr, "x"), list())

    expr = quote(mean(x[1:5]))
    expect_equal(find_var(expr, "x"), list(c(2, 2)))

    #TODO: Problem is the missing arg.
    expr = quote(plot(dframe[, "d"]))
    actual = find_var(expr, "dframe")

    expect_equal(actual, list(c(2, 2)))

    expr = quote(mean(x[1:5]) + x)
    actual = find_var(expr, "x")
    # I don't care about the order of the elements of this list.
    expect_equal(actual, list(c(2, 2, 2), 3))

    # Don't match character vectors
    expr = quote(paste("x", "y"))
    expect_equal(find_var(expr, "y"), list())

    expr = parse(text = '
        d = read.csv("data.csv")
        hist(d[, 2])
    ')
    actual = find_var(expr, "read.csv")

    expect_equal(actual, list(c(1, 3, 1)))


    expr = parse(text = '
        f = function(end, start = x) area(sin, start, end)
        f(x)
    ')

    # expr[[c(1, 3, 2, 2)]] = as.symbol("z")
    # TODO: Running the above results in `expr` still printing `x`, but
    # code evaluates as if it were changed to `z`.
    # I don't know what's happening.

    actual = find_var(expr, "x")

    expect_equal(actual, list(c(1, 3, 2, 2), c(2, 2)))

})


test_that("find_call", {

    e0 = quote(sapply(x, f))

#codedoctor:::find_call(e0, "lapply")

    expect_equal(find_call(e0, "lapply"), list())

    e1 = quote(lapply(x, f))
    expect_equal(find_call(e1, "lapply"), list(1L))

    e2 = quote(y <- lapply(x, f))
    expect_equal(find_call(e2, "lapply"), list(c(3L, 1L)))

    e3 = quote(y <- c(lapply(x, f1), lapply(x, f2)))
    expect_equal(find_call(e3, "lapply"), list(c(3L, 2L, 1L), c(3L, 3L, 1L)))

    e4 = quote(y <- lapply(lapply(x, f), g))
    expect_equal(find_call(e4, "lapply"), list(c(3L, 1L), c(3L, 2L, 1L)))

})


test_that("all_symbols", {

    e = quote(plot(x, y))
    actual = sort(all_symbols(e))
    expected = sort(c("plot", "x", "y"))

    expect_equal(actual, expected)

    # Using x as a function also. Yuck!
    e = parse(text = "x(plot(x, y))
              plot(x)")
    actual = sort(all_symbols(e))

    expect_equal(actual, expected)

})


test_that("tree methods", {

    tree = list(list(list(1, 2, 3), 4), 5)
    actual = tree[[c(1, 1, 2)]]
    expect_equal(actual, 2)

})


test_that("only_literals", {

    expect_true(only_literals(quote(1:5)))

    expect_true(only_literals(quote(c(1, 4))))

    expect_false(only_literals(quote(f(3))))

    expect_false(only_literals(quote(1:n)))

})


test_that("even_split", {

    actual = even_split(6, 2)    
    expect_equal(actual, c(1, 1, 1, 2, 2, 2))

    actual = even_split(7, 2)    
    expect_equal(actual, c(1, 1, 1, 1, 2, 2, 2))

})
