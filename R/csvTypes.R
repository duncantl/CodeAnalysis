# When doing static analysis of code, it is useful to find the information
# about a data file that is being read when we know the name of the file,
# i.e., it is a literal in the code.
# We then can get the 

# generalize for tabular Data. The methods will flow to tabularFileInfo
# which is general enough. Functions for

# Name tabularTypeInfo could include dimensions too, but excessively long.


if(FALSE) {
 tabularTypeInfo(quote(read.table("climateinputs5probs.csv", sep = ",", header = TRUE)), 20)
}

setGeneric("csvTypeInfo",
function(file, nrow = 10, ...)
    standardGeneric("csvTypeInfo"))

setMethod("csvTypeInfo", "character",
          function(file, nrow = 10, ...) {
              e =  substitute(read.csv(file, nrow = nrow), list(file = file, nrow = nrow))
              
              tabularFileInfo(e, ...)
          })

setMethod("csvTypeInfo", "call",
          function(file, nrow = 10, ...) {
              tabularFileInfo(file, nrow, ...)
          })

setGeneric("tabularTypeInfo",
             function (file, nrow = 10, ...)
           standardGeneric("tabularTypeInfo"))

setMethod("tabularTypeInfo", "call",
          function(file, nrow = 10, ...) {
              tabularFileInfo(file, nrow, ...)
          })


tabularFileInfo = 
function(expr, nrow = 10, ...)
{
    expr = matchCall(expr)
    nr = wc(as.character(expr[[2]]))
    
    d = eval(insertNRow(I(expr), nrow))
    nc = ncol(d)
    vars = names(d)
    
    structure(list(dim = c(nr, nc), elNames = vars, elementClasses = lapply(d, class), elementTypes = sapply(d, typeof)), class = "TabularTypeInfo")
}

insertNRow =
    #
    # insertNRow(quote(read.csv("climateinputs5probs.csv")), 20)
    # insertNRow(quote(read.csv("climateinputs5probs.csv",nrow = 10)), 20)
    # insertNRow(quote(read.csv("climateinputs5probs.csv",nrow = 10)), 5)
    #
    # insertNRow(quote(read.table("climateinputs5probs.csv", sep = ",")), 20)
    # insertNRow(quote(read.table("climateinputs5probs.csv", sep = ",", colClasses = c("integer", "character", NULL, NULL, "integer"))), 20)
function(expr, nrow)
{
    if(!is(expr, "AsIs"))
        expr = matchCall(expr)
    
    k = expr
    if(!("nrow" %in% names(k)) || is.symbol(k$nrow) || (is.numeric(k$nrow) && nrow < k$nrow))
      k$nrow = nrow
    k
}

matchCall =
function(expr, envir = globalenv())
{
    fun = get(as.character(expr[[1]]), envir)
    match.call(fun, expr)
}

wc =
function(file)
{
    if(!file.exists(file))
       stop("no such file ", file)
    
  as.integer(gsub(file, "", system(paste0("wc -l ", file), intern =TRUE))) - 1L
}
