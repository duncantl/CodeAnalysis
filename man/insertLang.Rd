\name{insertLang}
\alias{insertLang}
\title{Insert language/code object(s) into a function or any language object}
\description{
This function hides the details of inserting one or more language objects - a call, name/symbol or
expression - into another language object.
This involves interleaving the new language element between two subsets of the target language
object. One has to compute the two subsets and then combine them as \code{c(s1, new, s2)}.
Since the target is typically a pair-list, this is slightly more complicated.
This function hides the details and handles the cases where we are adding the new element at
the start of the code block, the end of the code, checking we are not adding it beyond the end
and also adding multiple language elements either contiguously or in different locations within
the target.

We can often use \code{\link[base]{trace}} to insert a call or an expression into a function.
\code{insertLang} helps in cases when \code{trace} doesn't work.

It is also useful when we are programmatically constructing or adapting functions.
}
\usage{
insertLang(expr, to, after = integer())
}
\arguments{
  \item{expr}{the language object to insert, or a list of language objects}
  \item{to}{the language object into which we want to insert \code{expr}.
    If this is a function, the function adds \code{expr} to the body of the function
   and returns the function. One can add a formal argument to a function directly.}
 \item{after}{the 1-based position at which to insert the new language object(s).
   If this is empty or \code{NA}, \code{expr} will be added at the end of \code{to}.
   If this is -1, the function inserts at the first position.
   If this is beyond the length of \code{to} the function issues a warning
   and adds the expression at the end of the code block \code{to}.
   If \code{expr} is a list of language objects, \code{after} can be a single
   position and the objects are inserted sequentially after that position,
   or else \code{after} should be an integer vector with as many elements as there are
   in \code{expr}. The positions should be the current positions in \code{to}
   and the function will account for the previous insertions.
   
 }
}
%\details{}
\value{
An updated version of \code{to} with the element(s) inserted.
}
\author{
Duncan Temple Lang
}
\seealso{
  \code{\link[base]{trace}}
}
\examples{

   f = function(x, y) 
          x + sin(y)

   f2 = insertLang(quote(print(c(length(x), length(y)))), f, -1)
   f2


   g = function(x, y) {
          w = !is.na(x)
          x = x[w]
          x + sin(y[w])
       }

   g2 = insertLang(quote(if(all(is.na(y[w]))) recover()), g, 3)

   g2 = insertLang(list( quote(print("hi")), quote(if(all(is.na(y[w]))) recover())), g, c(1, 3))

   g2 = insertLang(list( quote(print("hi")), quote(if(all(is.na(y[w]))) recover())), g, c(-1, 3))

   # 2 calls immediately after element 3
   g2 = insertLang(list( quote(print("hi")), quote(if(all(is.na(y[w]))) recover())), g, 3)

\dontrun{
 # Problem
 insertLang(quote(print(c(length(x), length(y)))), f, 0)
}
}
\keyword{programming}
\concept{metaprogramming}
