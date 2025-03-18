if(FALSE) {
    # Example.
    ns = getNamespace("tools")
    tfns = as.list(ns, all.names = TRUE)
    tfns = tfns[sapply(tfns, is.function)]

    ats = lapply(tfns, getAllAttributeNames)
    head(dsort(table(unlist(ats))), 20)
}

getAllAttributeNames =
function(x)    
{
    a = findCallsTo(x, "attr")
    ats = sapply(a, function(x) if(is.character(x[[3]])) as.character(x[[3]]) else NA)
    c(getAllAttributeNames.structure(x),
      unname(ats[!is.na(ats)])
     )
}

getAllAttributeNames.structure =
function(x)
{
    st = findCallsTo(x, "structure")
    attr = unlist(lapply(st, names)) 
    unname(attr[ attr != "" ])
}
