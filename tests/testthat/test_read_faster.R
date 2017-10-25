
test_that("data_read", {

              skip("don't care about these yet")

expect_equal(data_read(code[[1]]), quote(d))

expect_null(data_read(quote(f(x))))

})


test_that("update_indices", {

    map = c(11:13, 40)

    code = quote(dframe[condition, 11])
    actual = update_indices(code, list(4), map)

    expect_equal(actual, quote(dframe[condition, 1L]))

    # No locations to update
    expect_equal(update_indices(code, list(), map), code)

    code = quote(dframe[dframe[, 40L] > 10, 11:13])
    actual = update_indices(code, list(4, c(3, 2, 4)), map)

    expect_equal(actual, quote(dframe[dframe[, 4L] > 10, 1:3]))

    code = quote(plot(dframe[, c(11L, 13L)]))
    actual = update_indices(code, list(c(2, 4)), map)

    expect_equal(actual, quote(plot(dframe[, c(1L, 3L)])))

})


test_that("to_fread", {

    code = quote(read.csv("data.csv"))
    actual = to_fread(code, select = c(2L, 4L))

    expected = quote(data.table::fread("data.csv", select = c(2L, 4L)))

    expect_equal(actual, expected)

    code = quote(read.csv("data.csv", col.names = c("a", "b", "c", "d")))

    actual = to_fread(code, select = c(2L, 4L), remove_col.names = TRUE)

    expect_equal(actual, expected)

    # This is ambiguous if the length of the col.names is different from
    # the length of the select. I could check and raise an error.
    actual = to_fread(code, select = 1:4, remove_col.names = FALSE)

    expected = quote(data.table::fread("data.csv"
        , col.names = c("a", "b", "c", "d"), select = 1:4))

    expect_equal(actual, expected)

})


#test_that("infer_read_var", {
#
#    code = parse(text = '
#                 d = read.csv("data.csv")
#                 head(d[, 1])
#                 ')
#
#    actual = infer_read_var(code)
#
#    expected = list(list(varname = "d", colnames = NULL))
#
#    expect_equal(actual, expected)
#
#})


test_that("infer_colnames", {

    code = quote(read.table("data.csv", col.names = c("a", "b")))
    actual = infer_colnames(code)

    expect_equal(actual, c("a", "b"))
    
    code = quote(read.csv("tiny.csv"))
    actual = infer_colnames(code)

    expect_equal(actual, c("one", "two", "three"))

    code = quote(read.csv("some_file_that_doesnt_exist"))
    actual = infer_colnames(code)

    expect_null(actual, c("one", "two", "three"))

})



test_that("basic read_faster", {

    code = parse(text = '
        d = read.csv("data.csv")
        hist(d[, 2])
    ')

    actual = read_faster(code, varname = "d", colnames = letters, readfunc = "read.csv")

    actual2 = read_faster(code)

    expected = parse(text = '
        d = data.table::fread("data.csv", select = 2L)
        hist(d[, 1L])
    ')

    expect_equal(actual, expected)
    expect_equal(actual2, expected)

})


test_that("inference of variable arguments", {

    code = parse(text = '
        d = read.csv("data.csv")
        hist(d[, 2])
    ')

    expected = parse(text = '
        d = data.table::fread("data.csv", select = 2L)
        hist(d[, 1L])
    ')

    actual = read_faster(code)

    expect_equal(actual, expected)


})


test_that("read_faster with nested subsetting and $, [, [[", {

    code = parse(text = '
        d = read.csv("data.csv")
        plot(d$c, d[[1]])
        plot(d[d[, 6] > 5, 5:7])
    ')

    actual = read_faster(code, varname = "d", colnames = letters, readfunc = "read.csv")

    expected = parse(text = '
        d = data.table::fread("data.csv", select = c(1L, 3L, 5L, 6L, 7L))
        plot(d[, 2L], d[, 1L])
        plot(d[d[, 4L] > 5, 3:5])
    ')

    # The srcref info indicating the lines stays as an attribute.
    attributes(expected) = NULL

    expect_true(same_expr(actual, expected))

    code_implicit = parse(text = '
        d = read.table("data.csv", col.names = c("a", "b", "c", "d", "e", "f", "g", "h"))
        plot(d$c, d[[1]])
        plot(d[d[, 6] > 5, 5:7])
    ')

    actual = read_faster(code_implicit)

    expect_true(same_expr(actual, expected))

})
