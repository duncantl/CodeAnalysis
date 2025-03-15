o = options()
o = options("digits")
o = options()$digits
o = options()["digits"]
o = options()[["digits"]]
o = options()[c("digits")]
o = options()[c("digits", "warn")]
options()[ c("abc", "xyz") ]
options()[[ "abc" ]]

options(warn = 2)

options(x, "foo") <- 1

getOption("warn")
getOption("warn", NA)


