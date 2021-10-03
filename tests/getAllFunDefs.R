f <- function(x) {
    p <- function(x)
        class(x)[1] == "bob"
    lapply(x[sapply(x, p)], function(x) x+1)
}

if(FALSE)
    f = function(y) y+1

f = function(x) sin(x^2)


nest =
function(dir)
{
    ff = list.files(dir)
    lapply(ff, function(f) {

        e = parse(f)
        w = sapply(e, function(x) is.call(x) && is.name(x[[1]]) && as.character(x[[1]]) %in% c("=", "<-"))
        lapply(e[w], function(x) sapply(x[[3]], function(x) class(x)[1]))
    })
}

nestNames =
function(dir)
{
    ff = list.files(dir)
    f1 = function(f) {

        e = parse(f)
        f2 = function(x) is.call(x) && is.name(x[[1]]) && as.character(x[[1]]) %in% c("=", "<-")
        w = sapply(e, f2)
        lapply(e[w], function(x) { f3 = function(x) class(x)[1]
                                   sapply(x[[3]], f3)
                                 })
    }
    lapply(ff, f1)
}



if(FALSE) {
ff = list.files("~/R-devel3/src/library/tools/R", full = TRUE, pattern = "\\.[Rr]$")
fi = structure(lapply(ff,function(x) getAllFunctionDefs(parse(x))), names = basename(ff))
ns = lapply(fi, attr, "nestLevel")
mx = max(sapply(ns, max))
i = sapply(ns, max) == mx
nnames = mapply( function(x, nl) attr(x, "nestInfo")[nl == mx], fi[ i ], ns[i])



tools.funs = unlist(lapply(ff, function(f) getAllFunctionDefs(parse(f), nesting = FALSE)))
nt = sapply(tools.funs, numTerms)
which(nt == max(nt))
head(sort(nt, decreasing = TRUE))
#            .check_packages            .install_packages 
#                      15902                         4955 
#            .build_packages .check_package_CRAN_incoming 
#                       3125                         2875 
#          do_install_source                       Rd2txt 
#                       2423                         2304 

# Number of anonymous functions and total functions
sum(sapply(fi, function(x) sum(is.na(names(x)))))
sum(sapply(fi, length)) 
# So 403 out of 1544, a little over 25%

anon.nt = unlist(sapply(fi, function(x) sapply(x[is.na(names(x))], numTerms)))
summary(anon.nt)
quantile(anon.nt, .95)
# the largest is 397.
fi$sotools[[13]]
# That's quite big and it contains 6 nested functions one of which has 80 terms
# 4 of these are very, very similar, just change the name of the attr and class.
# So repeated code.

isAnon = which(is.na(names(fi$check.R)))
}
