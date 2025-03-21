if(FALSE) {
    # Example.
    ns = getNamespace("tools")
    tfns = as.list(ns, all.names = TRUE)
    tfns = tfns[sapply(tfns, is.function)]

    ats = lapply(tfns, getAttributeNames)
    head(dsort(table(unlist(ats))), 20)
}

# The order was not guaranteed as we get the attributes in structure() calls first
# and then attr() calls.
# We now arrange this with findCallsTo(x, c("attr", "structure"))
# and then process the calls depending on if it is to attr or structure.

getAttributeNames =
function(x)    
{
    a = findCallsTo(x, c("attr", "structure", "attributes"))
    ats = lapply(a, function(x) {
                           switch(as.character(x[[1]]),
                                  attr = if(is.character(x[[3]])) as.character(x[[3]]) else NA,
                                  structure = names(x)[names(x) != "" ],
                                  NA)
                    })
    ats = unlist(ats)
    ats[!is.na(ats)]
}

# Can leave but not exported and now not used by getAttributeNames() above.
getAttributeNames.structure =
function(x)
{
    st = findCallsTo(x, "structure")
    attr = unlist(lapply(st, names)) 
    unname(attr[ attr != "" ])
}
