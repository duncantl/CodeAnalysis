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


if(FALSE) {
    library(codetools)
    w = mkModifyCodeWalker(function(x, ...) x)
    ns = getNamespace("tools")
    tfns = as.list(ns, all.names = TRUE)
    # Takes 5 seconds for 794 objects
    ok = sapply(tfns, function(v) 
        try(identical(v, walkCode(v, w))))

    table(types <- sapply(tfns, typeof))
    #
    #  character     closure      double environment     integer        list     logical 
    #         26         716           2           5          12          32           1 


    # disable stop for now.
    stopifnot(all(ok))
    # Originally, 770 TRUE, 24 are not.

    table(types[!ok])
    v = names(tfns)[!ok]
    # 21 are C_... and are resolved native routine info object.
    # Other 4 were 
    # .standardizable_license_specs_db
    # IANA_HTTP_status_code_db
    # IANA_URI_scheme_db
    # Adobe_glyphs
    # Comparing
    #  a = tfns$Adobe_glyphs
    #  b = walkCode(a, w)
    #  all.equal(a, b)
    # we see it is the attributes. Hence, we set attributes(ans) = attributes(x) in
    # the mkModifyCodeWalker() function for the clause that processes the
    #   expression/list/language object.
    #
    # Not doing this for a pairlist. Setting attributes doesn't "stick" for formals().
    # Where else do we see pairlist in the wild?
    #
    # attr(formals(f), "xyz") = 10
    # is.null(attr(formals(f), "xyz"))
    #
    # But the following yields 10
    if(FALSE) {
     p = formals(f)
     attr(p, "xyz") = 10
     class(p)
     attr(p, "xyz") == 10
    }
    # 
    # But the
    if(FALSE) {
        formals(f) = p
        attributes(formals(f))
        attr(formals(f), "xyz")
    }
    
    #
}



