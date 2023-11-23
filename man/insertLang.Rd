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
insertLang(expr, to, after = integer(), includeBraceInCount = FALSE)
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
 \item{includeBraceInCount}{a logical value. By default, this is
   \code{FALSE} and means we don't include the \code{\{}  
   when counting the code elements in the original code.
   If we have 3 elements and the brace and \code{after = 2},
   the new code will be inserted after the 2nd code element.
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

   f0 = function(x, y) 
          x + sin(y)

       # the first two insert the new code at the start of the body of the function
   insertLang(quote(print(c(length(x), length(y)))), f0, -1)
   insertLang(quote(print(c(length(x), length(y)))), f0, 0)
       # this adds the code to the end of the body of the function
   insertLang(quote(print(c(length(x), length(y)))), f0)

# Problem. 
#XXX insertLang(quote(print(c(length(x), length(y)))), f0, 1)

   # A function with a body consisting of multiple expressions enclosed in a { }
   g = function(x, y) {
          w = !is.na(x)
          x = x[w]
          x + sin(y[w])
       }

    # insert single call
   g2 = insertLang(quote(if(all(is.na(y[w]))) recover()), g, 3)

    # insert multiple calls at different locations
   g2 = insertLang(list( quote(print("hi")), quote(if(all(is.na(y[w]))) recover())), g, c(1, 3))

   g2 = insertLang(list( quote(print("hi")), quote(if(all(is.na(y[w]))) recover())), g, c(-1, 3))

   # 2 calls immediately after element 3
   g2 = insertLang(list( quote(print("hi")), quote(if(all(is.na(y[w]))) recover())), g, 3)


   # Illustrate where the new code goes in different situations.
   # Default is to put code immediately after the "after" element counting by elements
   # and not including the  { in the count of positions.
   # So here we label the code elements e1, e2, ...
   # We insert new code elements named n1, n2 and n3
   # and you can see which  elements they are added after - with includeBraceInCount = FALSE and
   # then TRUE.

    f = function() {
        e1
        e2
        e3
        e4
        e5
    }
    nw = list(quote(n1), quote(n2), quote(n3))

    insertLang(nw[[1]], body(f), after = 3)
    insertLang(nw[[1]], body(f), after = -1)
    insertLang(nw[[1]], body(f))
    
    insertLang(nw, body(f), after = 3)
    insertLang(nw, body(f), after = c(2, 4, 5))
    insertLang(nw, body(f), after = -1)
    insertLang(nw, body(f))

     # with includeBraceInCount = TRUE
    insertLang(nw[[1]], body(f), after = 3, TRUE)

    insertLang(nw, body(f), after = 3, TRUE)
    insertLang(nw, body(f), after = c(2, 4, 5), TRUE)

     # includeBraceCount doesn't matter here as prepending to function body
    insertLang(nw[[1]], body(f), after = -1, TRUE)
    insertLang(nw, body(f), after = -1, TRUE)

     # includeBraceCount doesn't matter here as appending to end
    insertLang(nw[[1]], body(f), includeBraceInCount = TRUE)
    insertLang(nw, body(f), includeBraceInCount = TRUE)
}
\keyword{programming}
\concept{metaprogramming}
