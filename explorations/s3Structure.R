# Consider the energy package on CRAN.
# A quick grep of the text in the R/ directory shows many class<- assignments
# with the literal value htest or disco.
# Curiously, in the NAMESPACE fill, there are 3 S3method registrations: 1 for disco and 2 for kgroups.
# however kgroups isn't set as a class in this way. That is set within a call to structure().
#
# We'll use S3Assignments to find the

CRANDir = "~/CRAN2/Pkgs2"
rfiles = list.files(file.path(CRANDir, "energy/R"), full.names = TRUE, pattern = "\\.[RSqr]$")

library(rstatic)
library(CodeAnalysis)

k = function(f, code = parse(f), ast = to_ast(code)) {
    isFun = sapply(ast$contents, function(x) is(x, "Assignment") && is(x$read, "Function"))
    funs = lapply(ast$contents[isFun] , function(x) x$read)
    names(funs) = sapply(ast$contents[isFun], function(x) x$write$value)

    ans = lapply(funs, S3Assignments)
    ans[!sapply(ans, is.null)]
}

ff = lapply(rfiles, k)
names(ff) = rfiles
ff = ff[ sapply(ff, length) > 0 ]

