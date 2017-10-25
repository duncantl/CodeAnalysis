#!/usr/bin/env Rscript
 # Auto generated at  2017-10-25 11:18:43 


 fileRowSums  <- function (infile, outfile, ...) 
{
    X = read.table(infile, ...)
    Xs = rowSums(X)
    write.table(Xs, outfile, col.names = FALSE, row.names = FALSE)
}

args <- as.list(commandArgs(trailingOnly = TRUE))
do.call( fileRowSums , args)