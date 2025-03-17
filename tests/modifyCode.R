library(codetools)
library(CodeAnalysis)

z = function() {
    if (is.null(packages)) packages <- unique(utils::installed.packages(priority = "high")[1L, 
                                                                                           1L])
}

z = function() x[,1]
w = mkModifyCodeWalker(function(x, ...) x)
z2 = walkCode(z, w)
stopifnot(identical(z, z2))



tmp = mkModifyCodeWalker
z3 = walkCode(tmp, w)
stopifnot(identical(z3, tmp))



