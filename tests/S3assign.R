library(CodeAnalysis)

e = new.env()
f = system.file("sampleCode/S3Classes.R", package = "CodeAnalysis")
source(f, e)

a = S3Assignments(e$f1)
stopifnot(identical(a, list("Extent")))

a = S3Assignments(e$f2)
stopifnot(identical(a, list("Extent")))

a = S3Assignments(e$f3)
stopifnot(identical(a, list(c("Extent", "1D"))))

a = S3Assignments(e$f3.5)
stopifnot(identical(a, list(c("Extent", NA))))

a = S3Assignments(e$f4)
stopifnot(identical(a, list(c("Extent", "1D"))))

a = S3Assignments(e$f4.5)
stopifnot(identical(a, list(c("Extent", NA))))

a = S3Assignments(e$f5)
stopifnot(identical(a, list(c("Extent", NA))))
