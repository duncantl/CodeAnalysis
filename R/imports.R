pkgSymbolsUsed =
    #
    #  Find the symbols used in functions in package pkg
    #  that come from the package fromPkg. This is useful
    #  for refining the import() command in NAMESPACE to
    #  be a more lean, precise and less problematic
    #   importFrom(fromPkg, symbol1, symbol2, ...)
    #
    #  symbolsUsed("Rllvm", "Rffi")
    #
function(pkg, fromPkg, ns = getNamespace(pkg))
{
    vals = as.list(ns, all = TRUE)
    isfun = sapply(vals, is.function)
    funs = vals[isfun]
    g = lapply(funs, codetools::findGlobals)
    g = unique(unlist(g))
    o = setdiff(g, names(vals))

    vals2 = getNamespace(fromPkg)
    o[ o %in% names(vals2) ] # might be in other packages also.
}
