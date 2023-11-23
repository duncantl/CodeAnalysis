library(CodeAnalysis)

getCodeSyms = function(fun) paste(sapply(as.list(body(fun))[-1], as.character), collapse = ";")

nw = list(quote(n1), quote(n2), quote(n3))
f = function() e1


stopifnot( getCodeSyms( insertLang(nw[[1]], f) ) == "e1;n1" )
stopifnot( getCodeSyms( insertLang(nw[[1]], f, -1) )  == "n1;e1" )
stopifnot( getCodeSyms( insertLang(nw[[1]], f, 0) )  == "n1;e1" )

#XXXX
stopifnot( getCodeSyms( insertLang(nw[[1]], f, 2) )  == "e1;n1" )



#######


f = function() {
    e1
    e2
    e3
    e4
    e5
}


stopifnot(identical( getCodeSyms( insertLang(nw[[1]], f, after = 3) ), "e1;e2;e3;n1;e4;e5"))
stopifnot(identical( getCodeSyms( insertLang(nw[[1]], f, after = -1) ), "n1;e1;e2;e3;e4;e5"))
stopifnot(identical( getCodeSyms( insertLang(nw[[1]], f) ), "e1;e2;e3;e4;e5;n1"))

stopifnot(identical( getCodeSyms( insertLang(nw, f, after = 3) ), "e1;e2;e3;n1;n2;n3;e4;e5"))
stopifnot(identical( getCodeSyms( insertLang(nw, f, after = c(2, 4, 5)) ), "e1;e2;n1;e3;e4;n2;e5;n3"))
stopifnot(identical( getCodeSyms( insertLang(nw, f, after = -1) ), "n1;n2;n3;e1;e2;e3;e4;e5"))
stopifnot(identical( getCodeSyms( insertLang(nw, f) ), "e1;e2;e3;e4;e5;n1;n2;n3"))

# with includeBraceInCount = TRUE
stopifnot(identical( getCodeSyms( insertLang(nw[[1]], f, after = 3, TRUE) ), "e1;e2;n1;e3;e4;e5"))

stopifnot(identical( getCodeSyms( insertLang(nw, f, after = 3, TRUE) ), "e1;e2;n1;n2;n3;e3;e4;e5"))
stopifnot(identical( getCodeSyms( insertLang(nw, f, after = c(2, 4, 5), TRUE) ), "e1;n1;e2;e3;n2;e4;n3;e5"))

# includeBraceCount doesn't matter here as prepending to function body
stopifnot(identical( getCodeSyms( insertLang(nw[[1]], f, after = -1, TRUE) ), "n1;e1;e2;e3;e4;e5"))
stopifnot(identical( getCodeSyms( insertLang(nw, f, after = -1, TRUE) ), "n1;n2;n3;e1;e2;e3;e4;e5"))

# includeBraceCount doesn't matter here as appending to end
stopifnot(identical( getCodeSyms( insertLang(nw[[1]], f, includeBraceInCount = TRUE) ), "e1;e2;e3;e4;e5;n1"))
stopifnot(identical( getCodeSyms( insertLang(nw, f, includeBraceInCount = TRUE) ), "e1;e2;e3;e4;e5;n1;n2;n3"))
