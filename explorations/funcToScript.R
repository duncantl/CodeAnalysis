#' Convert a function into a script which can be run from command line
#'
#' Only handling functions which take character arguments
funcToScript = function(f, script = paste0(deparse(f), ".R"))
{

    # Assume it's not an anonymous function
    fname = deparse(f)

    # Print everything right to the script
    sink(script, type = "output")

    cat(
"#!/usr/bin/env Rscript\n"
, "Auto generated at ", as.character(Sys.time())
, "\n\n\n"
, fname, " <- "
)
print(f)
    cat("
args <- commandArgs(trailingOnly = TRUE)
do.call(", fname, ", args)"
)

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
