library(codetools)
library(CodeAnalysis)

f9 = function(n, B = 999) {
    replicate(B, { a = f(rnorm(n)); mean(g(a))})
}

fv = list(f = c("alpha" = "alpha1", "beta" = "beta1"), g = c("alpha" = "alpha2"))

rw = genAddArgsToCalls(fv)
w = mkModifyCodeWalker(rw, FALSE)
f2 = walkCode(f9, w)
