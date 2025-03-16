library(CodeAnalysis)

tmp = findUsedOptions(quote( o <- options()))
stopifnot(length(tmp) == 0)

tmp <- findUsedOptions(quote( o <- options("digits")))
stopifnot(tmp == "digits")


tmp <- findUsedOptions(quote(o <- options()$digits))
stopifnot(tmp == "digits")

tmp <- findUsedOptions(quote(o <- options()["digits"]))
stopifnot(tmp == "digits")

tmp <- findUsedOptions(quote(o <- options()[["digits"]]))
stopifnot(tmp == "digits")

tmp <- findUsedOptions(quote(o <- options()[c("digits")]))
stopifnot(tmp == "digits")

tmp <- findUsedOptions(quote(o <- options()[c("digits", "warn")]))
stopifnot(all(tmp == c("digits", "warn")))

tmp <- findUsedOptions(quote(options()[ c("abc", "xyz") ]))
stopifnot(all(tmp == c("abc", "xyz")))

tmp <- findUsedOptions(quote(options()[[ "abc" ]]))
stopifnot(tmp == "abc")

tmp <- findUsedOptions(quote(options(warn = 2)))
stopifnot(length(tmp) == 0)

tmp <- findUsedOptions(quote(options(other = "2")))
stopifnot(length(tmp) == 0)

tmp <- findUsedOptions(quote(options()[[x]]))

tmp <- findUsedOptions(quote(getOption("warn")))
stopifnot(tmp == "warn")

tmp <- findUsedOptions(quote(getOption("warn", NA)))
stopifnot(tmp == "warn")



