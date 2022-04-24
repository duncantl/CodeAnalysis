# Comes from NextLevelComputationalReasoning/HackingREngine/
# It extends (and calls) getAnywhere()

getAnywhere =
    #
    # if it finds multiple matches and one is in namespace X and another in package X,
    # then it, by default, discards the one in the namespace.
    # The keep argument specifies which to keep and if empty, keeps all the matches.
    #
    #
function(x, keep = "package")
{
    if(grepl("::", x)) {
        els = strsplit(x, ":::?")[[1]]
        return(list(objs = list(get(els[2], getNamespace(els[1])))))
    }
    trimAnywhere(utils::getAnywhere(x), keep)
}

trimAnywhere =
function(x, keep = character())
{
    if(length(keep) == 0 || length(x$where) == 0)
        return(x)

    dup = duplicated(x$objs)
    if(any(dup))
        x = subset(x, !dup)

    if(length(x$objs) == 1)
        return(x)

    els = strsplit(x$where, ":")
    v = data.frame( name = sapply(els, `[`, 2), type = sapply(els, `[`, 1), index = 1:length(els))
    i = unlist(by(v, v$name, function(x) if(any(x$type %in% keep)) x$index[x$type %in% keep] else x$index))
    subset.getAnywhere(x, i)
}

subset.getAnywhere =
function(x, i)    
{
    x$objs = x$objs[i]
    x$where = x$where[i]
    x$visible = x$visible[i]
    x$dups = x$dups[i]
    x
}
