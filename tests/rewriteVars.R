library(codetools)
library(CodeAnalysis)
f = function(x) {
    alpha *x + beta
}

rw = genRewriteVars(c(alpha = ".alpha", beta = ".beta"))
w = mkModifyCodeWalker(rw, FALSE)
f2 = walkCode(f, w)

