if(FALSE) {
    # Example.
    ns = getNamespace("tools")
    tfns = as.list(ns, all.names = TRUE)
    tfns = tfns[sapply(tfns, is.function)]

    ats = lapply(tfns, getAllAttributeNames)
    head(dsort(table(unlist(ats))), 20)
}

# The order is not guaranteed as we get the attributes in structure() calls first
# and then attr() calls.
# We could arrange this with findCallsTo(x, c("attr", "structure"))
# and then process the calls differently.

getAttributeNames =
function(x)    
{
    a = findCallsTo(x, "attr")
    ats = sapply(a, function(x) if(is.character(x[[3]])) as.character(x[[3]]) else NA)
    c(getAttributeNames.structure(x),
      unname(ats[!is.na(ats)])
     )
}

getAttributeNames.structure =
function(x)
{
    st = findCallsTo(x, "structure")
    attr = unlist(lapply(st, names)) 
    unname(attr[ attr != "" ])
}
