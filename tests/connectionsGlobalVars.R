library(CodeAnalysis)
foo =
function(file)
{
    con = textConnection("tmp", "w", local = TRUE)
    on.exit(close(con))
    knitr::knit(file, output = con, tangle = TRUE, quiet = TRUE)
    # should use textConnectionValue(con), but can access the local variable tmp.
    tmp
}

stopifnot(length(getGlobals(foo)$variables) == 0)
stopifnot(getGlobals(foo, handleTextConnections = FALSE)$variables == "tmp")
