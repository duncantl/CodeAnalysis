pkgSymbolsUsed =
    #
    #  Doesn't the tools package have a function that does this???
    #
    #  Find the symbols used in functions in package pkg
    #  that come from the package fromPkg. This is useful
    #  for refining the import() command in NAMESPACE to
    #  be a more lean, precise and less problematic
    #   importFrom(fromPkg, symbol1, symbol2, ...)
    #
    #  symbolsUsed("Rllvm", "Rffi")
    #
    #  XXX need to differentiate/identify from which package a particular
    #  function is being imported.
    #   If pkg A imports packages X and Y and both X and Y have a symbol foo,
    #   which one does A import  and can we get the provenance.
    #
    #  This is probably not getting the methods we import or the operators, e.g
    #  RAutoGenRunTime's & and  |.
    #
function(pkg, fromPkg, ns = getNamespace(pkg), exportedOnly = TRUE)
{
    vals = as.list(ns, all = TRUE)
    isfun = sapply(vals, is.function)
    funs = vals[isfun]
    g = lapply(funs, codetools::findGlobals) # should we use getGlobals() from this package.
    g = unique(unlist(g))
    o = setdiff(g, names(vals))

    vals2 = getNamespace(fromPkg)


    imports = ns$".__NAMESPACE__."$imports
    contains = function(x, val)  val %in% x

    # Do we need  to reverse the order of imports. base is first.
    from = sapply(o, function(var) names(imports)[which(sapply(imports, contains, var))[1]])
    names(from) = o
    w = !is.na(from)
    from = from[w]
    # Find the values which are from  fromPkg
    ans = names(from)[from == fromPkg]
    return(ans)

    #XXX
    if(exportedOnly) {
       exports = ls(vals2$".__NAMESPACE__."$exports, all.names = TRUE)
       o[ o %in% exports ]
   } else {
        fromPkg.imports = ns$".__NAMESPACE__."$imports[[fromPkg]]    
        o[ o %in% names(vals2) ] # might be in other packages also.
    }
}
