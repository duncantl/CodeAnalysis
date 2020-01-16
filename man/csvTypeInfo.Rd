\name{csvTypeInfo}
\alias{csvTypeInfo}
\alias{csvTypeInfo,character-method}
\alias{csvTypeInfo,call-method}

\title{Get information about CSV columns and number of rows}
\description{
  This function reads the first few rows of a CSV (comma-separate value)
  file and determines the classes of its columns and also
  gets the number of rows in the file.
  This gives us information about the types and dimensions of the
  contents  which we can use in static analysis.

  This assumes the first rows are representative  of the column types.
}
\usage{
csvTypeInfo(file, nrow = 10, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{the name of the CSV file, or a connection object, or a
    language/code object.}
  \item{nrow}{the number of rows to read to determine the types of the
    columns.  The larger the number, the greater the certainty of the
    resulting types but the slower the computation.}
  \item{\dots}{additional arguments for methods.}
}

\value{
  A list of class \code{TabularTypeInfo} with elements
  \item{dim}{a length 2 integer vector giving the number of rows and
  columns. The number of rows may be \code{NA}.}
  \item{elNames}{the names of the columns in the would-be \code{data.frame}}
  \item{elementClasses}{a list of character vectors with an element for
  each of the column names in \code{elNames}.}
  \item{elementTypes}{a character vector with the corresponding
  \code{\link[base]{typeof}} for each of the columns in the would-be \code{data.frame}.}
}
\author{
Duncan Temple Lang
}
\seealso{
 \code{RTypeInference} package at \url{https://github.com/duncantl/RTypeInference}.
}
\examples{
f = system.file("sampleCode/simple.csv", package = "CodeAnalysis")
csvTypeInfo(f)

# Gymnastics here that we would not ordinarily have to do except for
# we are evaluating this from within a help file.

cur = getwd()
setwd(dirname(f))

csvTypeInfo(quote(read.csv("simple.csv", colClasses = c("numeric", "NULL", "character"))))

setwd(cur)

# again, gymnastics to create the expression for the help file.

e = substitute(read.csv(f, colClasses = c("numeric", "NULL", "character")), list(f = f))
csvTypeInfo(e)
}
\keyword{IO}
\keyword{programming}
