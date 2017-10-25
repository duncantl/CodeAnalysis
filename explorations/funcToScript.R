#' Convert a function into a script which can be run from command line
#'
#' Only handling functions which take character arguments
#'
#' @param f function
#' @param fname name of the function. Must provide if f is anonymous
#' @param script file to write the script to
funcToScript = function(f
    , fname = deparse(substitute(f))
    , script = paste0(fname, ".R")
){
    # Print everything right to the script
    sink(script, type = "output")

    cat(
"#!/usr/bin/env Rscript\n"
, "# Auto generated at ", as.character(Sys.time())
, "\n\n\n"
, fname, " <- "
)
    print(f)
    cat("
args <- as.list(commandArgs(trailingOnly = TRUE))
do.call(", fname, ", args)"
)
    sink()
}


############################################################
# Testing
############################################################


#' Write Row Sums Of Infile To Outfile
fileRowSums = function(infile, outfile, ...)
{
    X = read.table(infile, ...)
    Xs = rowSums(X)
    write.table(Xs, outfile, col.names = FALSE, row.names = FALSE)
}

fileRowSums("123.txt", stdout())

funcToScript(fileRowSums)
