f <- function(x) {
    p <- function(x)
        class(x)[1] == "bob"
    lapply(x[sapply(x, p)], function(x) x+1)
}

if(FALSE)
    f = function(y) y+1

f = function(x) sin(x^2)

